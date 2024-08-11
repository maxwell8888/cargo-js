mod definition_data;
mod handle_syn_expr;
mod handle_syn_item;
mod handle_syn_stmt;

use definition_data::{resolve_path, ScopedVar};
pub use definition_data::{
    GlobalData, GlobalDataScope, RustImplItemItemJs, RustType2, RustTypeImplTrait2, RustTypeParam2,
    RustTypeParamValue2,
};
pub use handle_syn_item::js_stmts_from_syn_items;
pub use handle_syn_stmt::handle_stmt;
use tracing::{debug, info};

use crate::{
    js_ast::{DestructureObject, DestructureValue, Ident, LocalName},
    tree_structure::{update_definitons::ItemV2, ItemActual, ItemRef},
    update_item_definitions::{ItemDefinition, RustTypeParam, RustTypeParamValue},
};
use quote::quote;
use syn::{GenericArgument, Member, Pat, PathArguments, Type, TypeParamBound};

/// Converts a syn pat to our own similar type. Also adds vars to the current scope, we do this here because we might need to add multiple vars eg for Pat::Struct. We could potentially just go through the resultant LocalName afterwards to add the vars, but it seems more concise to to it here where we are going through the tree anyway.
///
/// Currently used by handle_local and handle_match
// fn handle_pat(pat: &Pat, scope: &mut GlobalDataScope, current_type: RustType) -> LocalName {
fn handle_pat(pat: &Pat, global_data: &mut GlobalData, current_type: RustType2) -> LocalName {
    let scope = global_data.scopes.last_mut().unwrap();
    match pat {
        Pat::Const(_) => todo!(),
        Pat::Ident(pat_ident) => {
            scope.variables.push(ScopedVar {
                name: pat_ident.ident.to_string(),
                mut_: pat_ident.mutability.is_some(),
                type_: current_type,
            });
            LocalName::Single(Ident::Syn(pat_ident.ident.clone()))
        }
        Pat::Lit(_) => todo!(),
        Pat::Macro(_) => todo!(),
        Pat::Or(_) => todo!(),
        Pat::Paren(_) => todo!(),
        Pat::Path(_) => todo!(),
        Pat::Range(_) => todo!(),
        Pat::Reference(_) => todo!(),
        Pat::Rest(_) => todo!(),
        Pat::Slice(pat_slice) => {
            //
            fn get_element_type(rust_type: RustType2) -> RustType2 {
                match rust_type {
                    RustType2::Vec(element_type) => *element_type,
                    RustType2::Array(element_type) => *element_type,
                    RustType2::MutRef(inner) => get_element_type(*inner),
                    other => {
                        dbg!(other);
                        todo!()
                    }
                }
            }
            let element_type = get_element_type(current_type);
            LocalName::DestructureArray(
                pat_slice
                    .elems
                    .iter()
                    .map(|elem| handle_pat(elem, global_data, element_type.clone()))
                    .collect::<Vec<_>>(),
            )
        }
        Pat::Struct(pat_struct) => {
            // TODO How are tuple structs destructured? Would they be a Pat::Tuple?

            let fields = pat_struct
                .fields
                .iter()
                .map(|field| {
                    let field_type = match &current_type {
                        RustType2::StructOrEnum(_type_params, item_def) => {
                            item_def.get_type(&field.member, global_data)
                        }
                        _ => todo!(),
                    };

                    // dbg!(&field.pat);
                    // dbg!(&field.member);
                    // dbg!(&field_type);
                    handle_destructure_pat(&field.pat, &field.member, global_data, field_type)
                })
                .collect::<Vec<_>>();
            LocalName::DestructureObject(DestructureObject(fields))
        }
        Pat::Tuple(_pat_tuple) => {
            todo!();
            // LocalName::DestructureArray(
            //     pat_tuple
            //         .elems
            //         .iter()
            //         .map(|elem| handle_pat(elem, global_data, current_type))
            //         .collect::<Vec<_>>(),
            // )
        }
        Pat::TupleStruct(_) => todo!(),
        Pat::Type(pat_type) => {
            // TODO does it make sense to also return the (parsed) RustType from handle_pat?
            handle_pat(&pat_type.pat, global_data, current_type)
        }
        Pat::Verbatim(_) => todo!(),
        // for `let _ = foo();` the lhs will be `Pat::Wild`
        Pat::Wild(_) => {
            // "in expressions, `_` can only be used on the left-hand side of an assignment" So we don't need to add _ to scope.vars
            LocalName::Single(Ident::Str("_"))
        }
        other => {
            dbg!(other);
            todo!();
        }
    }
}

fn handle_destructure_pat(
    pat: &Pat,
    member: &Member,
    global_data: &mut GlobalData,
    current_type: RustType2,
) -> DestructureValue {
    let scope = global_data.scopes.last_mut().unwrap();
    match pat {
        Pat::Const(_) => todo!(),
        Pat::Ident(pat_ident) => {
            info!(name = ?pat_ident.ident.to_string(), type_ = ?current_type, "push var");
            scope.variables.push(ScopedVar {
                name: pat_ident.ident.to_string(),
                mut_: pat_ident.mutability.is_some(),
                type_: current_type,
            });

            let pat_ident = pat_ident.ident.clone();
            let member_ident = match member.clone() {
                Member::Named(ident) => ident,
                Member::Unnamed(_) => todo!(),
            };
            if member_ident == pat_ident {
                DestructureValue::KeyName(Ident::Syn(member_ident))
            } else {
                DestructureValue::Rename(Ident::Syn(member_ident), Ident::Syn(pat_ident))
            }
        }
        Pat::Lit(_) => todo!(),
        Pat::Macro(_) => todo!(),
        Pat::Or(_) => todo!(),
        Pat::Paren(_) => todo!(),
        Pat::Path(_) => todo!(),
        Pat::Range(_) => todo!(),
        Pat::Reference(_) => todo!(),
        Pat::Rest(_) => todo!(),
        Pat::Slice(_) => todo!(),
        Pat::Struct(pat_struct) => {
            let member = match member.clone() {
                Member::Named(ident) => ident,
                Member::Unnamed(_) => todo!(),
            };
            let fields = pat_struct
                .fields
                .iter()
                .map(|field| {
                    let field_type = match &current_type {
                        RustType2::StructOrEnum(_type_params, item_def) => {
                            item_def.get_type(&field.member, global_data)
                        }
                        _ => todo!(),
                    };

                    handle_destructure_pat(&field.pat, &field.member, global_data, field_type)
                })
                .collect::<Vec<_>>();

            DestructureValue::Nesting(Ident::Syn(member), DestructureObject(fields))
        }
        Pat::Tuple(_) => todo!(),
        Pat::TupleStruct(_) => todo!(),
        Pat::Type(_) => todo!(),
        Pat::Verbatim(_) => todo!(),
        Pat::Wild(_) => todo!(),
        _ => todo!(),
    }
}

/// Only used when handling syn closures and locals
/// We can't use this for fns/methods *return types* because we need to actually parse the body to differentiate between self and another var with the type Self.
/// So in all we are (*should be) using this for parsing: field types (which are kind of like self anyway?), and fn/method inputs
/// * fn has "return_type" in the name...
///
/// Remember this is only for definitions, but the types used *within* definitions can have resolved generics
///
/// NOTE also using this to parse local stmt type annotations eg `let foo: i32 = 5;`
///
/// TODO also using this for return types in handle_item_fn and not sure if that is reasonable
/// TODO handle types with len > 1
///
///
/// 
/// TODO should be parsing these `syn::Type`s for closures and locals in update_definitions along with the item definitions
fn parse_fn_input_or_field(
    type_: &Type,
    has_mut_keyword: bool,
    // TODO if this is for passing types used in a parent item definition, surely the parent items generics cannot have been made concrete, in which case this should be &Vec<String> instead of &Vec<RustTypeParam>?
    parent_item_definition_generics: &[RustTypeParam],
    // TODO should just store the current module in GlobalData to save having to pass this around everywhere
    current_module: &[String],
    global_data: &mut GlobalData,
) -> RustType2 {
    todo!()
}

// return type for `handle_expr_path` because the path might not comprise a full expression/type, ie a tuple struct or enum variant that has args so requires being called
#[derive(Debug, Clone)]
enum PartialRustType {
    /// This is only used for tuple struct instantiation since normal struct instantiation are parsed to Expr::Struct and so can be directly evaluated to a struct instance, whereas a tuple struct instantiation is parsed as an ExprCall. Ok but Expr::Struct still has a `.path` `Path` field which we want to be able to parse/handle with the same handle_expr code, so now this can also be the path of a Expr::Struct
    ///
    /// So we are assuming that *all* cases where we have an Expr::Path and the final segment is a struct ident, it must be a tuple struct
    ///
    /// (type params, module path, name) module path is None for scoped structs
    StructIdent(Vec<RustTypeParam2>, ItemDefinition),
    /// This is only used for instantiation of enum variants with args which are parsed as an ExprCall, since normal enum variant instantiation are simply evaluated directly to an enum instance.
    /// Note we need to record type params because we might be parsing something like the `MyGenericEnum::<i32>::MyVariant` portion of `MyGenericEnum::<i32>::MyVariant("hi")` where the *enum definition* has had generics resolved
    ///
    /// (type params, module path, scope id, enum name, variant name) module path is None for scoped structs
    ///
    /// IMPORTANT NOTE this variant should only be used for tuple and struct variants, normal path variants should be a PartialRustType::RustType
    EnumVariantIdent(Vec<RustTypeParam2>, ItemDefinition, String),
    // TODO why no type params?
    /// (rust type, is mut, is var)
    RustType(RustType2, bool, bool),
}

#[derive(Debug, Clone)]
pub struct RustPathSegment2 {
    ident: String,
    turbofish: Vec<RustType2>,
}
