mod definition_data;
mod handle_syn_expr;
mod handle_syn_item;
mod handle_syn_stmt;

use std::rc::Rc;

use definition_data::{resolve_path, ScopedVar};
pub use definition_data::{
    GlobalData, GlobalDataScope, RustImplItemItemJs, RustType2, RustTypeImplTrait2, RustTypeParam2,
    RustTypeParamValue2,
};
pub use handle_syn_item::js_stmts_from_syn_items;
use tracing::info;

use crate::{
    js_ast::{DestructureObject, DestructureValue, Ident, LocalName},
    update_item_definitions::{
        ItemDef, ItemDefRc, RustTypeParam, RustTypeParamValue, StructEnumDef,
    },
};
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
    match type_ {
        Type::Array(_) => todo!(),
        Type::BareFn(_) => todo!(),
        Type::Group(_) => todo!(),
        Type::ImplTrait(type_impl_trait) => {
            // TODO handle len > 1
            let _type_param_bound = type_impl_trait.bounds.first().unwrap();
            // get_return_type_of_type_param_bound(
            //     type_param_bound,
            //     parent_item_definition_generics,
            //     current_module,
            //     global_data,
            // )
            let bounds = type_impl_trait
                .bounds
                .iter()
                .filter_map(|b| {
                    match b {
                        TypeParamBound::Trait(trait_bound) => {
                            // TODO need to resolve trait names/paths with the module they are defined in and store that so we can look them up properly
                            if trait_bound.path.segments.len() > 1 {
                                todo!();
                            }
                            // TODO handle segment arguments because we might have eg `impl GenericFooTrait<Bar>`
                            let trait_name = trait_bound
                                .path
                                .segments
                                .iter()
                                .map(|t| t.ident.to_string())
                                .collect::<Vec<_>>();
                            // let poo = trait_name.into_iter().map(|fart| fart);
                            // TODO lookup trait in global data to get module path
                            // let (module_path, trait_definition) = global_data
                            //     .lookup_trait_definition_any_module(current_module, trait_name);
                            let (trait_module_path, trait_item_path, trait_item_scope, item_index) =
                                resolve_path(
                                    true,
                                    false,
                                    true,
                                    true,
                                    trait_bound
                                        .path
                                        .segments
                                        .iter()
                                        .map(|t| RustPathSegment2 {
                                            ident: t.ident.to_string(),
                                            turbofish: Vec::new(),
                                        })
                                        .collect::<Vec<_>>(),
                                    &global_data.item_refs,
                                    &global_data.item_defs,
                                    current_module,
                                    current_module,
                                    &global_data.scopes,
                                );
                            // A Trait bound should just be a trait, no associated fn or whatever
                            assert!(trait_item_path.len() == 1);
                            let trait_def = match global_data.item_defs[item_index.unwrap()].clone()
                            {
                                ItemDefRc::Trait(trait_def) => trait_def,
                                _ => todo!(),
                            };

                            Some(RustTypeImplTrait2::SimpleTrait(trait_def))
                        }
                        TypeParamBound::Lifetime(_) => None,
                        TypeParamBound::Verbatim(_) => todo!(),
                        _ => todo!(),
                    }
                })
                .collect::<Vec<_>>();

            RustType2::ImplTrait(bounds)
        }
        Type::Infer(_) => todo!(),
        Type::Macro(_) => todo!(),
        Type::Never(_) => todo!(),
        Type::Paren(_) => todo!(),
        Type::Path(type_path) => {
            // eg:
            // Foo<i32>
            // Foo<T>
            // Self (which given we are dealing with field or input, *must* be a different instance/type from self)
            // T (where the types bounds of T are elsewhere eg `where T: FooTrait` or `<T: FooTrait>`)
            // T: impl FooTrait

            // If it is a field, a type with a generic like T or Foo<T> means the generic depends on the parent so for
            // let foo = Foo { gen_field: i32 };
            // let field = foo.gen_field;
            // If we have just stored that the type of gen_field is T, we can just resolve T to i32 because we will haev the type of foo which will contain the resolved generics.
            // What happens if it hasn't been resolved yet? eg it might get resolved by being passed as an argument to a fn, so the arg type defines it? Should be fine as long as know we have an unresolved generic, so can (should be able to) look up the input types for the fn/method
            if type_path.path.segments.len() == 1 {
                let seg = type_path.path.segments.first().unwrap();
                let seg_name = seg.ident.to_string();
                let seg_name_str = seg_name.as_str();

                // Look to see if name is a generic which has been declared
                let generic = parent_item_definition_generics
                    .iter()
                    .rev()
                    .find(|generic| generic.name == seg_name_str);
                if let Some(generic) = generic {
                    // return match &generic.type_ {
                    //     RustTypeParamValue::Unresolved => {
                    //         dbg!(generic);
                    //         todo!()
                    //     }
                    //     RustTypeParamValue::RustType(rust_type) => *rust_type.clone(),
                    // };
                    return RustType2::TypeParam(RustTypeParam2 {
                        name: generic.name.clone(),
                        type_: match generic.type_.clone() {
                            RustTypeParamValue::Unresolved => RustTypeParamValue2::Unresolved,
                            RustTypeParamValue::RustType(rust_type) => {
                                RustTypeParamValue2::RustType(Box::new(
                                    rust_type.into_rust_type2(global_data),
                                ))
                            }
                        },
                    });
                }

                // For fns:
                // the names of generics should be stored on FnInfo
                // Sometimes we can work out what the type of a generic is, eg it impls Fn in which case we only care about the return type, but mostly the generic will just be eg T, maybe with some trait bound, but that doesn't help us determine what the actual type is. We need to record where the generic type is inferred from, eg:
                // 1. the generic is used as the type for an input: easy, just check the type of the thing that eventually gets passed as an arg
                // 2. the generic is used as the return type: redundant, we don't need to know the type since we already determine the return type from the body

                // For structs
                // Can always be inferred from the arguments used to construct the struct?

                // For impl blocks
                match seg_name_str {
                    // "i32" => RustType::I32,
                    "bool" => RustType2::Bool,
                    // "str" => RustType::String,
                    "Option" => {
                        let generic_type = match &seg.arguments {
                            PathArguments::AngleBracketed(angle_bracketed_generic_arguments) => {
                                match angle_bracketed_generic_arguments.args.first().unwrap() {
                                    GenericArgument::Lifetime(_) => todo!(),
                                    GenericArgument::Type(type_) => parse_fn_input_or_field(
                                        type_,
                                        has_mut_keyword,
                                        parent_item_definition_generics,
                                        current_module,
                                        global_data,
                                    ),
                                    GenericArgument::Const(_) => todo!(),
                                    GenericArgument::AssocType(_) => todo!(),
                                    GenericArgument::AssocConst(_) => todo!(),
                                    GenericArgument::Constraint(_) => todo!(),
                                    _ => todo!(),
                                }
                            }
                            _ => todo!(),
                        };
                        RustType2::Option(RustTypeParam2 {
                            // TODO "T" shouldn't be hardcoded here
                            name: "T".to_string(),
                            type_: RustTypeParamValue2::RustType(Box::new(generic_type)),
                        })
                    }
                    // "RustInteger" => {RustType::Struct(StructOrEnum { ident: "RustInteger".to_string(), members: (), generics: (), syn_object: () }),
                    // "RustFloat" => RustType::Struct(StructOrEnum { ident: "RustFloat".to_string(), members: (), generics: (), syn_object: () }),
                    // "RustString" => RustType::Struct(StructOrEnum { ident: "RustString".to_string(), members: (), generics: (), syn_object: () }),
                    // "RustBool" => RustType::Struct(StructOrEnum { ident: "RustBool".to_string(), members: (), generics: (), syn_object: () }),
                    struct_or_enum_name => {
                        // dbg!(current_module);
                        // dbg!(&global_data.scope_id_as_option());
                        // dbg!(&vec![struct_or_enum_name.to_string()]);
                        // let (item_definition_module_path, item_definition, index) = global_data
                        //     .lookup_item_definition_any_module_or_scope(
                        //         current_module,
                        //         &[struct_or_enum_name.to_string()],
                        //     );
                        let (item_definition_module_path, item_path, _is_scoped, item_index) =
                            resolve_path(
                                true,
                                false,
                                true,
                                true,
                                vec![RustPathSegment2 {
                                    ident: struct_or_enum_name.to_string(),
                                    turbofish: Vec::new(),
                                }],
                                &global_data.item_refs,
                                &global_data.item_defs,
                                current_module,
                                current_module,
                                &global_data.scopes,
                            );
                        // A Trait bound should just be a trait, no associated fn or whatever
                        assert!(item_path.len() == 1);
                        let item_definition =
                            match global_data.item_defs[item_index.unwrap()].clone() {
                                ItemDefRc::StructEnum(trait_def) => trait_def,
                                _ => todo!(),
                            };

                        // Look to see if any of the item's type params have been specified (they *must* have been specified, because you can't use a type without specifiying prodviding it's type params so they must either be concrete types, or use one of the parents params)
                        let item_type_params = match &seg.arguments {
                            PathArguments::None => Vec::new(),
                            PathArguments::AngleBracketed(gen_args) => {
                                gen_args
                                    .args
                                    .iter()
                                    .enumerate()
                                    .filter_map(|(i, a)| match a {
                                        GenericArgument::Lifetime(_) => None,
                                        GenericArgument::Type(type_) => {
                                            // Get the name of the generic from the items definition
                                            let gen_arg_name = item_definition.generics[i].clone();

                                            // First check if type is one of the parent's generics - why? purely so that we know whether the type is a generic or concrete type, we could equally look up the concrete type and assume that if that fails it must be a generic
                                            let type_param = if let Some(parent_generic) =
                                                parent_item_definition_generics
                                                    .iter()
                                                    .find(|g| g.name == gen_arg_name)
                                            {
                                                match parent_generic.type_ {
                                                    RustTypeParamValue::Unresolved => {
                                                        RustTypeParam2 {
                                                            name: gen_arg_name,
                                                            type_: RustTypeParamValue2::Unresolved,
                                                        }
                                                    }
                                                    // "an item definition should not have any resovled type params"
                                                    RustTypeParamValue::RustType(_) => panic!(),
                                                }
                                            } else {
                                                // Otherwise we have another type we need to parse
                                                let param_rust_type = parse_fn_input_or_field(
                                                    type_,
                                                    has_mut_keyword,
                                                    parent_item_definition_generics,
                                                    current_module,
                                                    global_data,
                                                );
                                                RustTypeParam2 {
                                                    name: gen_arg_name,
                                                    type_: RustTypeParamValue2::RustType(Box::new(
                                                        param_rust_type,
                                                    )),
                                                }
                                            };
                                            Some(type_param)
                                        }
                                        GenericArgument::Const(_) => todo!(),
                                        GenericArgument::AssocType(_) => todo!(),
                                        GenericArgument::AssocConst(_) => todo!(),
                                        GenericArgument::Constraint(_) => todo!(),
                                        _ => todo!(),
                                    })
                                    .collect::<Vec<_>>()
                            }
                            PathArguments::Parenthesized(_) => todo!(),
                        };

                        // match item_definition.syn_object {
                        //     StructOrEnumSynObject::Struct(_) => RustType::StructOrEnum(item_type_params, item_module_path, item_definition.ident.to_string()),
                        //     StructOrEnumSynObject::Enum(_) => RustType::Enum(item_type_params, item_module_path, item_definition.ident.to_string()),
                        // }

                        if item_definition_module_path == vec!["prelude_special_case".to_string()] {
                            if item_definition.ident == "i32" {
                                if has_mut_keyword {
                                    global_data.rust_prelude_types.rust_integer = true;
                                }
                                RustType2::I32
                            } else if item_definition.ident == "String"
                                || item_definition.ident == "str"
                            {
                                if has_mut_keyword {
                                    global_data.rust_prelude_types.rust_string = true;
                                }
                                RustType2::String
                            } else {
                                todo!()
                            }
                        } else {
                            RustType2::StructOrEnum(item_type_params, item_definition)
                        }
                    }
                }
            } else {
                todo!()
            }
        }
        Type::Ptr(_) => todo!(),
        Type::Reference(type_reference) => {
            // let type_ = parse_type(&type_reference.elem);
            // let type_ = match type_ {
            //     TypeOrVar::RustType(rust_type) => rust_type,
            //     TypeOrVar::Unknown => RustType::Unknown,
            // };
            // TypeOrVar::Var(ScopedVar {
            //     name: "donotuse".to_string(),
            //     mut_: false,
            //     mut_ref: type_reference.mutability.is_some(),
            //     type_,
            // })
            let type_ = parse_fn_input_or_field(
                &type_reference.elem,
                has_mut_keyword,
                parent_item_definition_generics,
                current_module,
                global_data,
            );
            if type_reference.mutability.is_some() {
                match type_ {
                    RustType2::I32 => {
                        global_data.rust_prelude_types.rust_integer = true;
                    }
                    RustType2::F32 => todo!(),
                    RustType2::Bool => todo!(),
                    RustType2::String => {
                        global_data.rust_prelude_types.rust_string = true;
                    }
                    _ => {}
                }
                RustType2::MutRef(Box::new(type_))
            } else {
                // RustType::Ref(Box::new(type_))
                type_
            }
        }
        Type::Slice(_) => todo!(),
        Type::TraitObject(_) => todo!(),
        Type::Tuple(_) => todo!(),
        Type::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}

// return type for `handle_expr_path` because the path might not comprise a full expression/type, ie a tuple struct or enum variant that has args so requires being called
#[derive(Debug, Clone)]
enum PartialRustType {
    /// This is only used for tuple struct instantiation since normal struct instantiation are parsed to Expr::Struct and so can be directly evaluated to a struct instance, whereas a tuple struct instantiation is parsed as an ExprCall. Ok but Expr::Struct still has a `.path` `Path` field which we want to be able to parse/handle with the same handle_expr code, so now this can also be the path of a Expr::Struct
    ///
    /// So we are assuming that *all* cases where we have an Expr::Path and the final segment is a struct ident, it must be a tuple struct
    ///
    /// (type params, module path, name) module path is None for scoped structs
    StructIdent(Vec<RustTypeParam2>, Rc<StructEnumDef>),
    /// This is only used for instantiation of enum variants with args which are parsed as an ExprCall, since normal enum variant instantiation are simply evaluated directly to an enum instance.
    /// Note we need to record type params because we might be parsing something like the `MyGenericEnum::<i32>::MyVariant` portion of `MyGenericEnum::<i32>::MyVariant("hi")` where the *enum definition* has had generics resolved
    ///
    /// (type params, module path, scope id, enum name, variant name) module path is None for scoped structs
    ///
    /// IMPORTANT NOTE this variant should only be used for tuple and struct variants, normal path variants should be a PartialRustType::RustType
    EnumVariantIdent(Vec<RustTypeParam2>, Rc<StructEnumDef>, String),
    // TODO why no type params?
    /// (rust type, is mut, is var)
    RustType(RustType2, bool, bool),
}

#[derive(Debug, Clone)]
pub struct RustPathSegment2 {
    pub ident: String,
    pub turbofish: Vec<RustType2>,
}
