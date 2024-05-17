use biome_formatter::{FormatLanguage, IndentStyle, IndentWidth};
use biome_js_formatter::{context::JsFormatOptions, JsFormatLanguage};
use biome_js_parser::JsParserOptions;
use biome_js_syntax::JsFileSource;
use handle_syn_expr::{handle_expr, handle_expr_block, handle_expr_match};
use handle_syn_stmt::handle_stmt;
use heck::{AsKebabCase, AsLowerCamelCase, AsPascalCase};
use js_ast::{
    DestructureObject, DestructureValue, JsClass, JsExpr, JsFn, JsIf, JsLocal, JsModule, LocalName,
};
use quote::quote;
use std::{
    default,
    fmt::{self, Debug},
    fs,
    net::ToSocketAddrs,
    path::{Path, PathBuf},
};
use syn::{
    parenthesized, parse_macro_input, BinOp, DeriveInput, Expr, ExprAssign, ExprBlock, ExprCall,
    ExprClosure, ExprMatch, ExprMethodCall, ExprPath, Fields, FnArg, GenericArgument, GenericParam,
    Ident, ImplItem, ImplItemFn, Item, ItemConst, ItemEnum, ItemFn, ItemImpl, ItemMod, ItemStruct,
    ItemTrait, ItemUse, Lit, Local, Macro, Member, Meta, Pat, PathArguments, PathSegment,
    ReturnType, Stmt, TraitItem, Type, TypeParamBound, UnOp, UseTree, Visibility, WherePredicate,
};
use tracing::{debug, debug_span, info, span, warn};

mod handle_syn_expr;
mod handle_syn_item;
use crate::handle_syn_item::handle_item;
mod handle_syn_stmt;
mod js_ast;
pub mod prelude;
pub mod rust_prelude;

pub use js_ast::JsStmt;

// TODO need to handle expressions which return `()`. Probably use `undefined` for `()` since that is what eg console.log();, var x = 5;, etc returns;
// TODO preserve new lines so generated js is more readable
// TODO consider how to get RA/cargo check to analyze rust inputs in `testing/`
// TODO add assertions to output JS and run that JS to ensure assertions pass

// fn handle_item_use_tree(use_tree: &UseTree, sub_modules: &mut Vec<DestructureValue>) {
//     dbg!(use_tree);
//     match use_tree {
//         UseTree::Path(use_path) => {
//             let key = use_path.ident.to_string();
//             let value = handle_item_use_tree(&*use_path.tree, sub_modules, root_module_or_crate);
//             sub_modules.push(Des)
//         }
//         UseTree::Name(use_name) => sub_modules.push(use_name.ident.to_string()),
//         UseTree::Rename(_) => todo!(),
//         UseTree::Glob(_use_glob) => sub_modules.push("*".to_string()),
//         UseTree::Group(use_group) => use_group.items.iter().for_each(|item| match item {
//             UseTree::Path(_) => todo!(),
//             UseTree::Name(use_name) => sub_modules.push(use_name.ident.to_string()),
//             UseTree::Rename(_) => todo!(),
//             UseTree::Glob(_) => todo!(),
//             UseTree::Group(_) => todo!(),
//         }),
//     }
// }

fn tree_to_destructure_object(use_tree: &UseTree) -> DestructureObject {
    match use_tree {
        UseTree::Path(use_path) => DestructureObject(vec![DestructureValue::Nesting(
            case_convert(&use_path.ident),
            tree_to_destructure_object(&*use_path.tree),
        )]),
        UseTree::Name(use_name) => DestructureObject(vec![DestructureValue::KeyName(
            case_convert(&use_name.ident),
        )]),
        UseTree::Rename(_) => todo!(),
        UseTree::Glob(_) => DestructureObject(vec![]),
        UseTree::Group(use_group) => DestructureObject(
            use_group
                .items
                .iter()
                .map(|item| match item {
                    UseTree::Path(use_path) => DestructureValue::Nesting(
                        case_convert(&use_path.ident),
                        tree_to_destructure_object(&*use_path.tree),
                    ),
                    UseTree::Name(use_name) => {
                        DestructureValue::KeyName(case_convert(&use_name.ident))
                    }
                    UseTree::Rename(_) => todo!(),
                    UseTree::Glob(_) => todo!(),
                    UseTree::Group(_) => todo!(),
                })
                .collect::<Vec<_>>(),
        ),
    }
}

/// We want each of used items eg `use mod::sub_mod::{item1, item2, another_mod::item3}` to return the name of the item, and the path relative to the root module
///
/// relative_path (snake) is a temporary var for building the relative path
///
/// items is what gets stored in global_data  Vec<(item name (snake), relative path (snake))>
fn tree_parsing_for_boilerplate(
    use_tree: &UseTree,
    relative_path: &mut Vec<String>,
    items: &mut Vec<(String, Vec<String>)>,
) {
    match use_tree {
        UseTree::Path(use_path) => {
            relative_path.push(use_path.ident.to_string());
            tree_parsing_for_boilerplate(&*use_path.tree, relative_path, items);
        }
        UseTree::Name(use_name) => items.push((use_name.ident.to_string(), relative_path.clone())),
        UseTree::Rename(_) => todo!(),
        UseTree::Glob(_) => todo!(),
        UseTree::Group(use_group) => {
            for item in &use_group.items {
                match item {
                    UseTree::Path(use_path) => {
                        let mut new_relative_path = relative_path.clone();
                        new_relative_path.push(use_path.ident.to_string());
                        tree_parsing_for_boilerplate(
                            &*use_path.tree,
                            &mut new_relative_path,
                            items,
                        );
                    }
                    UseTree::Name(use_name) => {
                        items.push((use_name.ident.to_string(), relative_path.clone()));
                    }
                    UseTree::Rename(_) => todo!(),
                    UseTree::Glob(_) => todo!(),
                    UseTree::Group(_) => todo!(),
                }
            }
        }
    };
}

enum ItemUseModuleOrScope<'a> {
    ExternalCrate,
    Module(&'a mut ModuleData),
    Scope(&'a mut GlobalDataScope),
}
fn handle_item_use(
    item_use: &ItemUse,
    // current_module: Vec<String>,
    // is_module: bool,
    // global_data: &mut GlobalData,
    // modules: &mut Vec<ModuleData>,
    item_use_module_or_scope: ItemUseModuleOrScope,
) {
    let public = match item_use.vis {
        Visibility::Public(_) => true,
        _ => false,
    };

    let (root_module_or_crate, sub_modules) = match &item_use.tree {
        UseTree::Path(use_path) => {
            // let mut sub_modules: Vec<DestructureValue> = Vec::new();
            let root_module_or_crate = use_path.ident.to_string();

            let sub_modules = tree_to_destructure_object(&*use_path.tree);
            // let sub_modules = DestructureObject (sub_modules);
            // handle_item_use_tree(&*use_path.tree, &mut sub_modules),
            (root_module_or_crate, sub_modules)
        }
        // UseTree::Name(use_name) => sub_modules.push(use_name.ident.to_string()),
        // TODO need to consider what a simple `use foo` means, since for modules this would be preceeded by `mod foo` which has the same effect?
        UseTree::Name(use_name) => todo!(),
        _ => panic!("root of use trees are always a path or name"),
    };

    // handle globs
    if sub_modules.0.len() == 0 {
        // For now we are not handling globs but need to use them for using enum variants which we will then need to inject manually
        return;
    }

    // if root_module_or_crate == "ravascript" || root_module_or_crate == "crate" {
    //     match &sub_modules.0[0] {
    //         DestructureValue::KeyName(_) => {}
    //         DestructureValue::Rename(_, _) => {}
    //         DestructureValue::Nesting(name, _) => {
    //             if name == "prelude" {
    //                 return;
    //             }
    //         }
    //     }
    // }

    // TODO fix this mess
    if match sub_modules.0.first().unwrap() {
        DestructureValue::KeyName(_) => "",
        DestructureValue::Rename(_, _) => "",
        DestructureValue::Nesting(name, _) => name,
    } == "web"
    {
        if root_module_or_crate == "Sse" {
            // JsStmt::Raw(SSE_RAW_FUNC.to_string())
        } else {
            // JsStmt::Expr(JsExpr::Vanish, false)
        }
    } else if root_module_or_crate == "serde" || root_module_or_crate == "serde_json" {
        // JsStmt::Expr(JsExpr::Vanish, false)
    } else if root_module_or_crate == "crate" {
        // If we import something from our crate, inline it (probably what we want for external crates too?)
        // A much simpler plan for now is to force defining the type in the JS file, and then export, rather than the other way round
        // Get the name of the item to be inlined
        todo!()
    } else {
        // JsStmt::Local(JsLocal {
        //     public,
        //     export: false,
        //     type_: LocalType::Var,
        //     lhs: LocalName::DestructureObject(sub_modules),
        //     value: JsExpr::Path(vec![camel(root_module_or_crate)]),
        // })

        // eg this.colorModule.spinachMessage = this.colorModule.greenModule.spinachModule.spinachMessage;

        let (_root_module_or_crate, item_paths) = match &item_use.tree {
            UseTree::Path(use_path) => {
                // let mut sub_modules: Vec<DestructureValue> = Vec::new();
                let root_module_or_crate = use_path.ident.to_string();

                // Vec<(item name (snake), relative path (snake))>
                let mut item_paths = Vec::new();
                let mut relative_path = vec![use_path.ident.to_string()];
                tree_parsing_for_boilerplate(&*use_path.tree, &mut relative_path, &mut item_paths);
                // let sub_modules = DestructureObject (sub_modules);
                // handle_item_use_tree(&*use_path.tree, &mut sub_modules),
                (root_module_or_crate, item_paths)
            }
            // UseTree::Name(use_name) => sub_modules.push(use_name.ident.to_string()),
            // TODO need to consider what a simple `use foo` means, since for modules this would be preceeded by `mod foo` which has the same effect?
            UseTree::Name(_use_name) => todo!(),
            _ => panic!("root of use trees are always a path or name"),
        };

        // TODO we do want to do the JsLocal destructure thing if the use is not a top level item?
        // TODO this is probably also the correct place to determine if std stuff like HashMap needs flagging
        // if is_module {
        match item_use_module_or_scope {
            ItemUseModuleOrScope::ExternalCrate => {}
            ItemUseModuleOrScope::Module(module) => {
                for item_path in item_paths {
                    // Get current module since it must already exist if we are in it
                    match item_use.vis {
                        Visibility::Public(_) => module.pub_use_mappings.push(item_path),
                        Visibility::Restricted(_) => todo!(),
                        Visibility::Inherited => module.private_use_mappings.push(item_path),
                    }
                }
            }
            ItemUseModuleOrScope::Scope(scope) => {
                for item_path in item_paths {
                    // Get current module since it must already exist if we are in it
                    match item_use.vis {
                        // TODO I believe the `pub` keyword for scoped `use` statements is irrelevant/redundant given that idents from scoped `use` statements aren't visible outside the scope. The only time the are relevant is if there is also a *scoped* module inside the same scope, but this seems pretty niche so we will not handle this case for now.
                        Visibility::Public(_) => todo!(),
                        Visibility::Restricted(_) => todo!(),
                        Visibility::Inherited => scope.use_mappings.push(item_path),
                    }
                }
            }
        }
    }
}

fn handle_destructure_pat(
    pat: &Pat,
    member: &Member,
    global_data: &mut GlobalData,
    current_type: RustType,
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

            let pat_ident = pat_ident.ident.to_string();
            let member = match member {
                Member::Named(ident) => ident.to_string(),
                Member::Unnamed(_) => todo!(),
            };
            if member == pat_ident {
                DestructureValue::KeyName(camel(member))
            } else {
                DestructureValue::Rename(camel(member), camel(pat_ident))
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
            let member = match member {
                Member::Named(ident) => ident.to_string(),
                Member::Unnamed(_) => todo!(),
            };
            let fields = pat_struct
                .fields
                .iter()
                .map(|field| {
                    let field_type = match &current_type {
                        RustType::StructOrEnum(type_params, module_path, name) => {
                            let item_def = global_data
                                .lookup_item_def_known_module_assert_not_func(&module_path, &name);
                            match item_def.struct_or_enum_info {
                                StructOrEnumDefitionInfo::Struct(struct_def_info) => {
                                    match struct_def_info.fields {
                                        StructFieldInfo::UnitStruct => todo!(),
                                        StructFieldInfo::TupleStruct(_) => todo!(),
                                        StructFieldInfo::RegularStruct(fields2) => fields2
                                            .iter()
                                            .find_map(|(field_name, field_type)| {
                                                let field_member_name = match &field.member {
                                                    Member::Named(ident) => ident.to_string(),
                                                    Member::Unnamed(_) => todo!(),
                                                };
                                                (field_name == &field_member_name)
                                                    .then_some(field_type)
                                            })
                                            .unwrap()
                                            .clone(),
                                    }
                                }
                                StructOrEnumDefitionInfo::Enum(_) => todo!(),
                            }
                        }
                        _ => todo!(),
                    };

                    handle_destructure_pat(&field.pat, &field.member, global_data, field_type)
                })
                .collect::<Vec<_>>();

            DestructureValue::Nesting(camel(member), DestructureObject(fields))
        }
        Pat::Tuple(_) => todo!(),
        Pat::TupleStruct(_) => todo!(),
        Pat::Type(_) => todo!(),
        Pat::Verbatim(_) => todo!(),
        Pat::Wild(_) => todo!(),
        _ => todo!(),
    }
}

/// Converts a syn pat to our own similar type. Also adds vars to the current scope, we do this here because we might need to add multiple vars eg for Pat::Struct. We could potentially just go through the resultant LocalName afterwards to add the vars, but it seems more concise to to it here where we are going through the tree anyway.
///
/// Currently used by handle_local and handle_match
// fn handle_pat(pat: &Pat, scope: &mut GlobalDataScope, current_type: RustType) -> LocalName {
fn handle_pat(pat: &Pat, global_data: &mut GlobalData, current_type: RustType) -> LocalName {
    let scope = global_data.scopes.last_mut().unwrap();
    match pat {
        Pat::Const(_) => todo!(),
        Pat::Ident(pat_ident) => {
            scope.variables.push(ScopedVar {
                name: pat_ident.ident.to_string(),
                mut_: pat_ident.mutability.is_some(),
                type_: current_type,
            });
            LocalName::Single(camel(&pat_ident.ident))
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
            fn get_element_type(rust_type: RustType) -> RustType {
                match rust_type {
                    RustType::Vec(element_type) => *element_type,
                    RustType::Array(element_type) => *element_type,
                    RustType::MutRef(inner) => get_element_type(*inner),
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
                        RustType::StructOrEnum(type_params, module_path, name) => {
                            let item_def = global_data
                                .lookup_item_def_known_module_assert_not_func(&module_path, &name);
                            match item_def.struct_or_enum_info {
                                StructOrEnumDefitionInfo::Struct(struct_def_info) => {
                                    match struct_def_info.fields {
                                        StructFieldInfo::UnitStruct => todo!(),
                                        StructFieldInfo::TupleStruct(_) => todo!(),
                                        StructFieldInfo::RegularStruct(fields) => fields
                                            .iter()
                                            .find_map(|(field_name, field_type)| {
                                                let field_member_name = match &field.member {
                                                    Member::Named(ident) => ident.to_string(),
                                                    Member::Unnamed(_) => todo!(),
                                                };
                                                (field_name == &field_member_name)
                                                    .then_some(field_type)
                                            })
                                            .unwrap()
                                            .clone(),
                                    }
                                }
                                StructOrEnumDefitionInfo::Enum(_) => todo!(),
                            }
                        }
                        _ => todo!(),
                    };

                    handle_destructure_pat(&field.pat, &field.member, global_data, field_type)
                })
                .collect::<Vec<_>>();
            LocalName::DestructureObject(DestructureObject(fields))
        }
        Pat::Tuple(pat_tuple) => {
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
            todo!();
            handle_pat(&pat_type.pat, global_data, current_type)
        }
        Pat::Verbatim(_) => todo!(),
        // for `let _ = foo();` the lhs will be `Pat::Wild`
        Pat::Wild(_) => {
            // "in expressions, `_` can only be used on the left-hand side of an assignment" So we don't need to add _ to scope.vars
            LocalName::Single("_".to_string())
        }
        other => {
            dbg!(other);
            todo!();
        }
    }
}

/// If TypeParamBound is a fn type, return it's return type otherwise return RustType::ImplTrait
// fn get_return_type_of_type_param_bound(
//     type_param_bound: &TypeParamBound,
//     generics: &Vec<RustTypeParam>,
//     current_module: &Vec<String>,
//     global_data: &GlobalData,
// ) -> RustType {
//     match type_param_bound {
//         TypeParamBound::Trait(trait_bound) => {
//             let seg = trait_bound.path.segments.first().unwrap();
//             let ident = &seg.ident;
//             // TODO currently only handling fn cases like `impl FnOnce(T) -> bool`
//             let is_fn = ident == "Fn" || ident == "FnMut" || ident == "FnOnce";
//             if is_fn {
//                 let return_type = match &seg.arguments {
//                     PathArguments::None => todo!(),
//                     PathArguments::AngleBracketed(_) => todo!(),
//                     PathArguments::Parenthesized(parenthesized_generic_arguments) => {
//                         match &parenthesized_generic_arguments.output {
//                             ReturnType::Default => todo!(),
//                             ReturnType::Type(_, type_) => parse_fn_input_or_field(
//                                 &*type_,
//                                 generics,
//                                 current_module,
//                                 global_data,
//                             ),
//                         }
//                     }
//                 };
//                 RustType::Fn(Box::new(return_type))
//             } else {
//                 RustType::ImplTrait
//             }
//         }
//         TypeParamBound::Lifetime(_) => todo!(),
//         TypeParamBound::Verbatim(_) => todo!(),
//         _ => todo!(),
//     }
// }

/// What is this used for? When storing info item *definitions*, specifically all the "members", like fields, methods, etc, and the members type/return type, we use this to go from syn::Type to MemberType
///
/// We can't use this for fns/methods *return types* because we need to actually parse the body to differentiate between self and another var with the type Self.
/// So in all we are (*should be) using this for parsing: field types (which are kind of like self anyway?), and fn/method inputs
/// * fn has "return_type" in the name...
///
/// Remember this is only for definitions, but the types used *within* definitions can have resolved generics
// fn parse_item_definition_return_type(
//     return_type: &Type,
//     generics: &Vec<RustTypeParam>,
//     // TODO should just store the current module in GlobalData to save having to pass this around everywhere
//     current_module: &Vec<String>,
//     global_data: &GlobalData,
// ) -> RustType {
//     match return_type {
//         Type::Array(_) => todo!(),
//         Type::BareFn(_) => todo!(),
//         Type::Group(_) => todo!(),
//         Type::ImplTrait(type_impl_trait) => {
//             let traits = type_impl_trait.bounds.iter().map(|type_param_bound| {
//                 match type_param_bound {
//                     TypeParamBound::Trait(trait_bound) => {
//                         let seg = trait_bound.path.segments.first().unwrap();
//                         let ident = &seg.ident;
//                         // TODO currently only handling fn cases like `impl FnOnce(T) -> bool`
//                         let is_fn = ident == "Fn" || ident == "FnMut" || ident == "FnOnce";
//                         if is_fn {
//                             // TODO is seg.arguments not the fn inputs rather than the return type?
//                             let return_type = match &seg.arguments {
//                                 PathArguments::None => todo!(),
//                                 PathArguments::AngleBracketed(_) => todo!(),
//                                 PathArguments::Parenthesized(parenthesized_generic_arguments) => {
//                                     match &parenthesized_generic_arguments.output {
//                                         ReturnType::Default => todo!(),
//                                         ReturnType::Type(_, type_) => parse_item_definition_return_type(
//                                             &*type_,
//                                             generics,
//                                             current_module,
//                                             global_data,
//                                         ),
//                                     }
//                                 }
//                             };
//                             // RustType::Fn(Box::new(return_type))
//                             RustTypeImplTrait::Fn(return_type)
//                         } else {
//                             if trait_bound.path.segments.len() > 1 {
//                                 todo!("handle impl traits::that::are::Paths");
//                             }
//                             RustTypeImplTrait::SimpleTrait(ident.to_string())
//                         }
//                     }
//                     TypeParamBound::Lifetime(_) => todo!(),
//                     TypeParamBound::Verbatim(_) => todo!(),
//                     _ => todo!(),
//                 }
//             }).collect::<Vec<_>>();

//             RustType::ImplTrait(traits)
//         }
//         Type::Infer(_) => todo!(),
//         Type::Macro(_) => todo!(),
//         Type::Never(_) => todo!(),
//         Type::Paren(_) => todo!(),
//         Type::Path(type_path) => {
//             if type_path.path.segments.len() == 1 {
//                 let seg = type_path.path.segments.first().unwrap();
//                 let seg_name = seg.ident.to_string();
//                 let seg_name_str = seg_name.as_str();

//                 let generics = match seg.arguments {
//                     PathArguments::None => Vec::new(),
//                     PathArguments::AngleBracketed(_) => todo!(),
//                     PathArguments::Parenthesized(_) => todo!(),
//                 }
//                 dbg!(&generics);
//                 // Look to see if name is a generic which has been declared
//                 let type_param_name = generics
//                     .iter()
//                     .rev()
//                     .find(|generic| generic == &&seg_name);

//                 if let Some(name) = type_param_name {
//                     return RustType::TypeParam(RustTypeParam { name: name.clone(), type_: RustTypeParamValue::Unknown });
//                 }

//                 // For fns:
//                 // the names of generics should be stored on FnInfo
//                 // Sometimes we can work out what the type of a generic is, eg it impls Fn in which case we only care about the return type, but mostly the generic will just be eg T, maybe with some trait bound, but that doesn't help us determine what the actual type is. We need to record where the generic type is inferred from, eg:
//                 // 1. the generic is used as the type for an input: easy, just check the type of the thing that eventually gets passed as an arg
//                 // 2. the generic is used as the return type: redundant, we don't need to know the type since we already determine the return type from the body

//                 // For structs
//                 // Can always be inferred from the arguments used to construct the struct?

//                 // For impl blocks

//                 match seg_name_str {
//                     "i32" => RustType::I32,
//                     "bool" => RustType::Bool,
//                     // TODO for now we are assuming T is a generic but we really need to determine whether a (length = 1 ?) Path is a generic variable/argument by looking to see if there is an Item defined with that name, and if not we can infer it is a generic variable/argument - NO - any generic used will have been declared in the parent item eg `fn foo<Bar>()` so we only need to keep track of which generics are available in which scopes.
//                     "T" => RustType::TypeParam(RustTypeParam {
//                         name: "T".to_string(),
//                         type_: RustTypeParamValue::Unknown,
//                     }),
//                     "str" => RustType::String,
//                     "Option" => {
//                         let generic_type = match &seg.arguments {
//                             PathArguments::AngleBracketed(angle_bracketed_generic_arguments) => {
//                                 match angle_bracketed_generic_arguments.args.first().unwrap() {
//                                     GenericArgument::Lifetime(_) => todo!(),
//                                     GenericArgument::Type(type_) => parse_fn_input_or_return_type(
//                                         type_,
//                                         generics,
//                                         current_module,
//                                         global_data,
//                                     ),
//                                     GenericArgument::Const(_) => todo!(),
//                                     GenericArgument::AssocType(_) => todo!(),
//                                     GenericArgument::AssocConst(_) => todo!(),
//                                     GenericArgument::Constraint(_) => todo!(),
//                                     _ => todo!(),
//                                 }
//                             }
//                             _ => todo!(),
//                         };
//                         RustType::Option(Box::new(generic_type))
//                     }
//                     // "RustInteger" => {RustType::Struct(StructOrEnum { ident: "RustInteger".to_string(), members: (), generics: (), syn_object: () }),
//                     // "RustFloat" => RustType::Struct(StructOrEnum { ident: "RustFloat".to_string(), members: (), generics: (), syn_object: () }),
//                     // "RustString" => RustType::Struct(StructOrEnum { ident: "RustString".to_string(), members: (), generics: (), syn_object: () }),
//                     // "RustBool" => RustType::Struct(StructOrEnum { ident: "RustBool".to_string(), members: (), generics: (), syn_object: () }),
//                     struct_or_enum_name => {
//                         let se = global_data
//                             .lookup_struct_or_enum(struct_or_enum_name.to_string(), current_module)
//                             .unwrap();
//                         match se.syn_object {
//                             StructOrEnumSynObject::Struct(_) => RustType::Struct(se),
//                             StructOrEnumSynObject::Enum(_) => RustType::Enum(se),
//                         }
//                     }
//                 }
//             } else {
//                 todo!()
//             }
//         }
//         Type::Ptr(_) => todo!(),
//         Type::Reference(type_reference) => {
//             // let type_ = parse_type(&type_reference.elem);
//             // let type_ = match type_ {
//             //     TypeOrVar::RustType(rust_type) => rust_type,
//             //     TypeOrVar::Unknown => RustType::Unknown,
//             // };
//             // TypeOrVar::Var(ScopedVar {
//             //     name: "donotuse".to_string(),
//             //     mut_: false,
//             //     mut_ref: type_reference.mutability.is_some(),
//             //     type_,
//             // })
//             let mut type_ = parse_fn_input_or_return_type(
//                 &type_reference.elem,
//                 generics,
//                 current_module,
//                 global_data,
//             );
//             if type_reference.mutability.is_some() {
//                 type_ = RustType::MutRef(Box::new(type_));
//             }
//             type_
//         }
//         Type::Slice(_) => todo!(),
//         Type::TraitObject(_) => todo!(),
//         Type::Tuple(_) => todo!(),
//         Type::Verbatim(_) => todo!(),
//         _ => todo!(),
//     }
// }

/// We can't use this for fns/methods *return types* because we need to actually parse the body to differentiate between self and another var with the type Self.
/// So in all we are (*should be) using this for parsing: field types (which are kind of like self anyway?), and fn/method inputs
/// * fn has "return_type" in the name...
///
/// Remember this is only for definitions, but the types used *within* definitions can have resolved generics
///
/// TODO also using this for return types in handle_item_fn and not sure if that is reasonable
/// TODO handle types with len > 1
fn parse_fn_input_or_field(
    type_: &Type,
    has_mut_keyword: bool,
    // TODO if this is for passing types used in a parent item definition, surely the parent items generics cannot have been made concrete, in which case this should be &Vec<String> instead of &Vec<RustTypeParam>?
    parent_item_definition_generics: &Vec<RustTypeParam>,
    // TODO should just store the current module in GlobalData to save having to pass this around everywhere
    current_module: &Vec<String>,
    global_data: &mut GlobalData,
) -> RustType {
    debug!(type_ = ?quote! { #type_ }.to_string(), "parse_fn_input_or_field");
    match type_ {
        Type::Array(_) => todo!(),
        Type::BareFn(_) => todo!(),
        Type::Group(_) => todo!(),
        Type::ImplTrait(type_impl_trait) => {
            // TODO handle len > 1
            let type_param_bound = type_impl_trait.bounds.first().unwrap();
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
                            // TODO lookup trait in global data to get module path
                            let (module_path, trait_definition) = global_data
                                .lookup_trait_definition_any_module(&trait_name, current_module)
                                .unwrap();
                            Some((
                                module_path,
                                RustTypeImplTrait::SimpleTrait(trait_definition.name.clone()),
                            ))
                        }
                        TypeParamBound::Lifetime(_) => None,
                        TypeParamBound::Verbatim(_) => todo!(),
                        _ => todo!(),
                    }
                })
                .collect::<Vec<_>>();

            RustType::ImplTrait(bounds)
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
                    return match &generic.type_ {
                        RustTypeParamValue::Unresolved => todo!(),
                        RustTypeParamValue::RustType(rust_type) => *rust_type.clone(),
                    };
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
                    "bool" => RustType::Bool,
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
                        RustType::Option(Box::new(generic_type))
                    }
                    // "RustInteger" => {RustType::Struct(StructOrEnum { ident: "RustInteger".to_string(), members: (), generics: (), syn_object: () }),
                    // "RustFloat" => RustType::Struct(StructOrEnum { ident: "RustFloat".to_string(), members: (), generics: (), syn_object: () }),
                    // "RustString" => RustType::Struct(StructOrEnum { ident: "RustString".to_string(), members: (), generics: (), syn_object: () }),
                    // "RustBool" => RustType::Struct(StructOrEnum { ident: "RustBool".to_string(), members: (), generics: (), syn_object: () }),
                    struct_or_enum_name => {
                        let (item_definition_module_path, item_definition) = global_data
                            .lookup_item_definition_any_module(
                                current_module,
                                &vec![struct_or_enum_name.to_string()],
                            )
                            .unwrap();

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
                                                        RustTypeParam {
                                                            name: gen_arg_name,
                                                            type_: RustTypeParamValue::Unresolved,
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
                                                RustTypeParam {
                                                    name: gen_arg_name,
                                                    type_: RustTypeParamValue::RustType(Box::new(
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

                        if item_definition_module_path
                            == Some(vec!["prelude_special_case".to_string()])
                        {
                            if item_definition.ident == "i32" {
                                if has_mut_keyword {
                                    global_data.rust_prelude_types.rust_integer = true;
                                }
                                RustType::I32
                            } else if item_definition.ident == "String"
                                || item_definition.ident == "str"
                            {
                                if has_mut_keyword {
                                    global_data.rust_prelude_types.rust_string = true;
                                }
                                RustType::String
                            } else {
                                todo!()
                            }
                        } else {
                            RustType::StructOrEnum(
                                item_type_params,
                                item_definition_module_path,
                                item_definition.ident.to_string(),
                            )
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
                    RustType::I32 => {
                        global_data.rust_prelude_types.rust_integer = true;
                    }
                    RustType::F32 => todo!(),
                    RustType::Bool => todo!(),
                    RustType::String => {
                        global_data.rust_prelude_types.rust_string = true;
                    }
                    _ => {}
                }
                RustType::MutRef(Box::new(type_))
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

/// Similar to parse_fn_input_or_field but for the extract_data_populate_item_definitions() pass before parsing, so only dealing with top level items, so don't need to check for scoped item definitions, also given we are popualting `.item_definitions()` etc, we need to avoid using these
///
/// Suitable for parsing: fn input types, fn return type, struct fields, enum variants with args
///
/// NOTE global data is required by get_path_without_namespacing which only uses pub_definitions etc, not `ItemDefintion`s
fn parse_types_for_populate_item_definitions(
    type_: &Type,
    // NOTE this will simply be empty for items that can't be generic, ie consts, or can but simply don't have any
    root_parent_item_definition_generics: &Vec<String>,
    // TODO should just store the current module in GlobalData to save having to pass this around everywhere
    current_module: &Vec<String>,
    global_data: &GlobalData,
) -> RustType {
    // let current_module_data = global_data
    //     .modules
    //     .iter()
    //     .find(|m| &m.path == current_module)
    //     .unwrap();
    match type_ {
        Type::Array(_) => todo!(),
        Type::BareFn(_) => todo!(),
        Type::Group(_) => todo!(),
        Type::ImplTrait(type_impl_trait) => {
            debug!(type_ = ?type_, "parse_fn_input_or_field Type::ImplTrait");
            // TODO handle len > 1
            // let type_param_bound = type_impl_trait.bounds.first().unwrap();
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
                            // TODO handle segment arguments because we might have eg `impl GenericFooTrait<Bar>`
                            let trait_bound_path = trait_bound
                                .path
                                .segments
                                .iter()
                                .map(|seg| RustPathSegment {
                                    ident: seg.ident.to_string(),
                                    turbofish: match &seg.arguments {
                                        PathArguments::None => Vec::new(),
                                        PathArguments::AngleBracketed(args) => args
                                            .args
                                            .iter()
                                            .map(|arg| match arg {
                                                GenericArgument::Lifetime(_) => todo!(),
                                                GenericArgument::Type(arg_type_) => {
                                                    parse_types_for_populate_item_definitions(
                                                        arg_type_,
                                                        root_parent_item_definition_generics,
                                                        current_module,
                                                        global_data,
                                                    )
                                                }
                                                GenericArgument::Const(_) => todo!(),
                                                GenericArgument::AssocType(_) => todo!(),
                                                GenericArgument::AssocConst(_) => todo!(),
                                                GenericArgument::Constraint(_) => todo!(),
                                                _ => todo!(),
                                            })
                                            .collect::<Vec<_>>(),
                                        PathArguments::Parenthesized(_) => todo!(),
                                    },
                                })
                                .collect::<Vec<_>>();
                            // TODO lookup trait in global data to get module path
                            let (trait_module_path, trait_item_path, _is_scoped) = get_path(
                                false,
                                true,
                                true,
                                trait_bound_path,
                                global_data,
                                current_module,
                                current_module,
                            );
                            // A Trait bound should just be a trait, no associated fn or whatever
                            assert!(trait_item_path.len() == 1);

                            // let (module_path, trait_definition) = global_data
                            //     .lookup_trait_definition_any_module(&trait_name, current_module)
                            //     .unwrap();
                            Some((
                                Some(trait_module_path),
                                RustTypeImplTrait::SimpleTrait(trait_item_path[0].ident.clone()),
                            ))
                        }
                        TypeParamBound::Lifetime(_) => None,
                        TypeParamBound::Verbatim(_) => todo!(),
                        _ => todo!(),
                    }
                })
                .collect::<Vec<_>>();

            RustType::ImplTrait(bounds)
        }
        Type::Infer(_) => todo!(),
        Type::Macro(_) => todo!(),
        Type::Never(_) => todo!(),
        Type::Paren(_) => todo!(),
        Type::Path(type_path) => {
            debug!(type_ = ?type_, "parse_fn_input_or_field Type::Path");
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

            let seg = type_path.path.segments.first().unwrap();
            let seg_name = seg.ident.to_string();
            let seg_name_str = seg_name.as_str();

            // Look to see if name is a generic which has been declared
            let generic = root_parent_item_definition_generics
                .iter()
                .find(|generic| generic == &seg_name_str);
            if let Some(generic) = generic {
                // return match &generic.type_ {
                //     RustTypeParamValue::Unresolved => todo!(),
                //     RustTypeParamValue::RustType(rust_type) => *rust_type.clone(),
                // };
                return RustType::TypeParam(RustTypeParam {
                    name: generic.clone(),
                    type_: RustTypeParamValue::Unresolved,
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
                "i32" => RustType::I32,
                "bool" => RustType::Bool,
                "str" => RustType::String,
                // TODO Option should be added to module/global data so we can handle it like any other item and also handle it properly if is has been shadowed
                "Option" => {
                    let generic_type = match &seg.arguments {
                        PathArguments::AngleBracketed(angle_bracketed_generic_arguments) => {
                            // Option only has
                            match angle_bracketed_generic_arguments.args.first().unwrap() {
                                GenericArgument::Lifetime(_) => todo!(),
                                GenericArgument::Type(type_) => {
                                    parse_types_for_populate_item_definitions(
                                        type_,
                                        root_parent_item_definition_generics,
                                        current_module,
                                        global_data,
                                    )
                                }
                                GenericArgument::Const(_) => todo!(),
                                GenericArgument::AssocType(_) => todo!(),
                                GenericArgument::AssocConst(_) => todo!(),
                                GenericArgument::Constraint(_) => todo!(),
                                _ => todo!(),
                            }
                        }
                        _ => todo!(),
                    };
                    RustType::Option(Box::new(generic_type))
                }
                // "RustInteger" => {RustType::Struct(StructOrEnum { ident: "RustInteger".to_string(), members: (), generics: (), syn_object: () }),
                // "RustFloat" => RustType::Struct(StructOrEnum { ident: "RustFloat".to_string(), members: (), generics: (), syn_object: () }),
                // "RustString" => RustType::Struct(StructOrEnum { ident: "RustString".to_string(), members: (), generics: (), syn_object: () }),
                // "RustBool" => RustType::Struct(StructOrEnum { ident: "RustBool".to_string(), members: (), generics: (), syn_object: () }),
                _ => {
                    // get full path
                    // NOTE only the final segment should have turbofish, or the final two if the path is an associated item
                    // NOTE also, get_path_without_namespacing() only preserves `RustPathSeg`s/turbofish, it doesn't use or update them so we could just populate them later

                    let rust_path = type_path
                        .path
                        .segments
                        .iter()
                        .map(|seg| RustPathSegment {
                            ident: seg.ident.to_string(),
                            turbofish: match &seg.arguments {
                                PathArguments::None => Vec::new(),
                                PathArguments::AngleBracketed(args) => {
                                    args.args
                                        .iter()
                                        .enumerate()
                                        .filter_map(|(i, arg)| match arg {
                                            GenericArgument::Lifetime(_) => None,
                                            GenericArgument::Type(arg_type_) => {
                                                let rust_type =
                                                    parse_types_for_populate_item_definitions(
                                                        arg_type_,
                                                        root_parent_item_definition_generics,
                                                        current_module,
                                                        global_data,
                                                    );

                                                // TODO IMPORTANT there is no easy way to get the names of the item generics here since we are still construction `ItemDefinition`s. However:
                                                // 1. Do we even need generic names after they have been resolved like here (generic types used within parent definitions must have concrete types (or the a generic of the parent) supplied)?
                                                // 2. We could just grab the generic names in the previous pass and add to .pub_definitions etc since this is as easy to get as the idents
                                                Some(RustTypeParam {
                                                    name: "unknown_todo".to_string(),
                                                    type_: RustTypeParamValue::RustType(Box::new(
                                                        rust_type,
                                                    )),
                                                })
                                            }
                                            GenericArgument::Const(_) => todo!(),
                                            GenericArgument::AssocType(_) => todo!(),
                                            GenericArgument::AssocConst(_) => todo!(),
                                            GenericArgument::Constraint(_) => todo!(),
                                            _ => todo!(),
                                        })
                                        .collect::<Vec<_>>();
                                    todo!()
                                }
                                PathArguments::Parenthesized(_) => todo!(),
                            },
                        })
                        .collect::<Vec<_>>();

                    let (item_module_path, item_path_seg, _is_scoped) = get_path(
                        false,
                        true,
                        true,
                        rust_path,
                        global_data,
                        current_module,
                        current_module,
                    );
                    let item_seg = &item_path_seg[0];

                    // NOTE for now we are assuming the type must be a struct or enum. fn() types will get matched by Type::BareFn not Type::Path, and traits should only appear in Type::ImplTrait. However we need to handle associated items eg `field: <MyStruct as MyTrait>::some_associated_type` which is a Path but to a type, not necessarily a struct/enum.
                    RustType::StructOrEnum(
                        item_seg
                            .turbofish
                            .iter()
                            .map(|rt| RustTypeParam {
                                name: "unknown_todo".to_string(),
                                type_: RustTypeParamValue::RustType(Box::new(rt.clone())),
                            })
                            .collect::<Vec<_>>(),
                        Some(item_module_path),
                        item_seg.ident.clone(),
                    )
                }
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
            let type_ = parse_types_for_populate_item_definitions(
                &type_reference.elem,
                root_parent_item_definition_generics,
                current_module,
                global_data,
            );
            if type_reference.mutability.is_some() {
                RustType::MutRef(Box::new(type_))
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

/// CONST_NAMES -> CONST_NAMES
/// PascalCase -> PascalCase
/// snake_case -> snakeCase
fn case_convert(name: impl ToString) -> String {
    let name = name.to_string();

    if name.chars().all(|c| c.is_uppercase() || c == '_') {
        // NOTE JS seems to be okay with uppercase keywords so don't need to convert
        name
    } else if name.chars().next().unwrap().is_ascii_uppercase() {
        // TODO this is redundant?
        AsPascalCase(name).to_string()
    } else {
        camel(name)
    }
}

fn camel(text: impl ToString) -> String {
    let text = text.to_string();

    // rename JavaScript keywords
    let text = if text == "default" {
        "default_vzxyw".to_string()
    } else {
        text
    };

    let underscore_prefix = text.starts_with("_");
    let camel = AsLowerCamelCase(text).to_string();
    if underscore_prefix {
        format!("_{camel}")
    } else {
        camel
    }
}

// In some cases we are only extracting the type, in others we have more info because we are extracting an existing variable or there is also info about the mutability, so wrap in this enum for convenience
// TODO probably shouldn't be using ScopedVar though because we will never extract the name or `mut` here
// TODO revisit if this enum is necessary or the best approach
#[derive(Clone, Debug)]
enum TypeOrVar {
    RustType(RustType),
    Var(ScopedVar),
    Unknown,
}
// #[derive(Clone, Debug)]
// enum FnReturnType {
//     /// (&mut , Type)
//     RustType(bool, RustType),
//     Unknown,
// }

// fn get_type_from_expr(
//     expr: &Expr,

//     global_data: &mut GlobalData,
//     current_module_path: &Vec<String>,
// ) -> RustType {
//     match expr {
//         Expr::Array(_) => RustType::Unknown,
//         Expr::Assign(_) => RustType::Unknown,
//         Expr::Async(_) => RustType::Unknown,
//         Expr::Await(_) => RustType::Unknown,
//         Expr::Binary(_) => RustType::Unknown,
//         Expr::Block(_) => RustType::Unknown,
//         Expr::Break(_) => RustType::Unknown,
//         Expr::Call(expr_call) => {
//             match &*expr_call.func {
//                 Expr::Path(expr_path) => {
//                     if expr_path.path.segments.len() == 1 {
//                         // If rhs is a single path fn call, look up the fn name in scopes to find it's return type
//                         let fn_name = expr_path.path.segments.first().unwrap().ident.to_string();
//                         global_data
//                             .scopes
//                             .iter()
//                             .rev()
//                             .find_map(|scope| {
//                                 let item_fn = scope
//                                     .fns
//                                     .iter()
//                                     .rev()
//                                     .find(|item_fn| item_fn.ident == fn_name);
//                                 item_fn.map(|item_fn| {
//                                     // We found a fn so try and parse it's return type
//                                     item_fn.return_type.clone()
//                                 })
//                             })
//                             // .unwrap_or(TypeOrVar::Unknown)
//                             .unwrap_or(RustType::Unknown)
//                     } else {
//                         // TypeOrVar::Unknown
//                         RustType::Unknown
//                     }
//                 }
//                 // _ => TypeOrVar::Unknown,
//                 _ => RustType::Unknown,
//             }
//         }
//         Expr::Cast(_) => RustType::Unknown,
//         Expr::Closure(_) => RustType::Unknown,
//         Expr::Const(_) => RustType::Unknown,
//         Expr::Continue(_) => RustType::Unknown,
//         Expr::Field(_) => RustType::Unknown,
//         Expr::ForLoop(_) => RustType::Unknown,
//         Expr::Group(_) => RustType::Unknown,
//         Expr::If(_) => RustType::Unknown,
//         Expr::Index(_) => RustType::Unknown,
//         Expr::Infer(_) => RustType::Unknown,
//         Expr::Let(_) => RustType::Unknown,
//         Expr::Lit(expr_lit) => {
//             match expr_lit.lit {
//                 Lit::Str(_) => RustType::String,
//                 Lit::ByteStr(_) => RustType::Todo,
//                 Lit::Byte(_) => RustType::Todo,
//                 Lit::Char(_) => RustType::Todo,
//                 // TODO need to know exact int type to know: 1. which Trait impl to use 2. whether to parse to JS BigInt
//                 Lit::Int(_) => RustType::I32,
//                 Lit::Float(_) => RustType::F32,
//                 Lit::Bool(_) => RustType::Bool,
//                 Lit::Verbatim(_) => RustType::Todo,
//                 _ => RustType::Unknown,
//             }
//         }
//         Expr::Loop(_) => RustType::Unknown,
//         Expr::Macro(_) => RustType::Unknown,
//         Expr::Match(_) => RustType::Unknown,
//         Expr::MethodCall(_) => RustType::Unknown,
//         Expr::Paren(_) => RustType::Unknown,
//         Expr::Path(expr_path) => {
//             if expr_path.path.segments.len() == 1 {
//                 if let Some(var) = global_data.scopes.iter().rev().find_map(|scope| {
//                     scope.variables.iter().rev().find(|var| {
//                         let name = expr_path.path.segments.first().unwrap().ident.to_string();
//                         var.name == name
//                     })
//                 }) {
//                     // TypeOrVar::Var(var.clone())
//                     var.type_.clone()
//                 } else {
//                     RustType::Unknown
//                 }
//             } else {
//                 RustType::Unknown
//             }
//         }
//         Expr::Range(_) => RustType::Unknown,
//         Expr::Reference(_) => RustType::Unknown,
//         Expr::Repeat(_) => RustType::Unknown,
//         Expr::Return(_) => RustType::Unknown,
//         Expr::Struct(_) => RustType::Unknown,
//         Expr::Try(_) => RustType::Unknown,
//         Expr::TryBlock(_) => RustType::Unknown,
//         Expr::Tuple(_) => RustType::Unknown,
//         Expr::Unary(expr_unary) => {
//             let deref = match expr_unary.op {
//                 UnOp::Deref(_) => true,
//                 _ => false,
//             };
//             let mut rust_type =
//                 get_type_from_expr(&*expr_unary.expr, global_data, current_module_path);
//             if deref {
//                 match rust_type {
//                     RustType::MutRef(rust_type2) => rust_type = *rust_type2.clone(),
//                     _ => {}
//                 }
//             }
//             rust_type
//         }
//         Expr::Unsafe(_) => RustType::Unknown,
//         Expr::Verbatim(_) => RustType::Unknown,
//         Expr::While(_) => RustType::Unknown,
//         Expr::Yield(_) => RustType::Unknown,
//         _ => RustType::Unknown,
//     }
// }

fn js_stmts_from_syn_items2(
    items: Vec<Vec<Item>>,
    // Need to keep track of which module we are currently in, for constructing the boilerplate
    current_module: &mut Vec<String>,
    global_data: &mut GlobalData,
) -> Vec<JsStmt> {
    let span = debug_span!("js_stmts_from_syn_items", current_module = ?current_module);
    let _guard = span.enter();

    let mut js_stmts = Vec::new();
    // let mut modules = Vec::new();
    // TODO this should be optional/configurable, might not always want it

    // We need to know what the syn type is to know whether it is eg a use which needs adding to the boiler plate
    // but we also need to put the struct impls into the class
    // solution:
    // push use to boilerplate in handle_item() and don't push the item to js_stmts
    // push other items as normal
    // after loop, group together classes
    // now that classes are grouped, we can add them to the module object - we either do the conversion to module objects right at the end after parsing all branches, otherwise we are always going to have to be able to amend the classes because impls might be in other modules.

    // remember that `impl Foo` can appear before `struct Foo {}` so classes definitely need multiple passes or to init class when we come across an impl, and then place it and add other data when we reach the actual struct definition
    // What happens when a method impl is outside the class's module? Could just find the original class and add it, but what if the method if using items from *it's* module? Need to replace the usual `this.someItem` with eg `super.someItem` or `subModule.someItem`. So we need to be able to find classes that appear in other modules

    for item in items {
        // handle_item(item, global_data, current_module, &mut js_stmts);
    }

    js_stmts
}

// TODO remove this as it is unnecessary redirection
/// Converts a Vec<syn::Item> to Vec<JsStmt> and moves method impls into their class
///
/// all users (eg crate, fn, file) want to group classes, but only crates want to populate boilerplate
fn js_stmts_from_syn_items(
    items: Vec<Item>,
    // Need to keep track of which module we are currently in, for constructing the boilerplate
    current_module: &mut Vec<String>,
    global_data: &mut GlobalData,
) -> Vec<JsStmt> {
    let span = debug_span!("js_stmts_from_syn_items", current_module = ?current_module);
    let _guard = span.enter();

    let mut js_stmts = Vec::new();
    // let mut modules = Vec::new();
    // TODO this should be optional/configurable, might not always want it

    // We need to know what the syn type is to know whether it is eg a use which needs adding to the boiler plate
    // but we also need to put the struct impls into the class
    // solution:
    // push use to boilerplate in handle_item() and don't push the item to js_stmts
    // push other items as normal
    // after loop, group together classes
    // now that classes are grouped, we can add them to the module object - we either do the conversion to module objects right at the end after parsing all branches, otherwise we are always going to have to be able to amend the classes because impls might be in other modules.

    // remember that `impl Foo` can appear before `struct Foo {}` so classes definitely need multiple passes or to init class when we come across an impl, and then place it and add other data when we reach the actual struct definition
    // What happens when a method impl is outside the class's module? Could just find the original class and add it, but what if the method if using items from *it's* module? Need to replace the usual `this.someItem` with eg `super.someItem` or `subModule.someItem`. So we need to be able to find classes that appear in other modules

    for item in items {
        handle_item(item, global_data, current_module, &mut js_stmts);
    }

    js_stmts
}

#[derive(Default, Clone, Debug)]
struct RustPreludeTypes {
    vec: bool,
    hash_map: bool,
    // TODO need to check which fns return an Option, and set `option` for those
    option: bool,
    some: bool,
    // TODO Is this just null?
    none: bool,
    result: bool,
    ok: bool,
    err: bool,
    assert_eq: bool,
    assert_ne: bool,
    dbg: bool,
    // println: bool,
    // print: bool,
    number_prototype_extensions: bool,
    string_prototype_extensions: bool,
    boolean_prototype_extensions: bool,
    integer: bool,
    float: bool,
    string: bool,
    bool: bool,
    equals: bool,

    // New approach
    // Basic `RustInteger`
    rust_integer: bool,
    rust_string: bool,
    rust_array_copy: bool,
}

#[derive(Debug, Clone)]
enum JsImplItem {
    /// This means that `foo() {}` will be used in place of `function foo() {}`  
    ///
    /// Some means it is a method, the first bool is whether it is private and thus should have # prepended to the name, the second bool is whether it is static  
    ///
    /// (class name, private, static, JsFn)  
    ClassMethod(String, bool, bool, JsFn),
    ClassStatic(JsLocal),
}
#[derive(Debug, Clone)]
struct ImplItemTemp {
    /// snake case
    class_name: String,
    /// ie method name or const name. snake case
    item_name: String,
    /// snake case
    module_path: Vec<String>,
    item_stmt: JsImplItem,
    // TODO what is this for can we delete it?
    // return_type: RustType,
}

// Third party crates
#[derive(Debug, Clone)]
struct CrateData {
    name: String,
    // Ideally we would just store the data like this, but we need to be able to resolve third party crate use statements, which might chain use statements, using `get_path_without_namespacing` just like any other module, so we need to maintain the same data structure? Yes we need to parse the third party crate anyway since we need to include it's source the JS output so will already have all it's ModuleData. Although in theory we could just do a one off calculation of all it's crate level pub module paths/items and only look for those when resolving paths in the main crate, which would reduce work, for now we will just resolve the paths just like any other module
    // (name, module path, definition)
    // items: Vec<(String, Vec<String>, ItemDefinition)>,
    modules: Vec<ModuleData>,
}

#[derive(Debug, Clone)]
struct ModuleData {
    name: String,
    parent_name: Option<String>,
    path: Vec<String>,
    pub_definitions: Vec<String>,
    private_definitions: Vec<String>,
    pub_submodules: Vec<String>,
    private_submodules: Vec<String>,
    /// (snake case item name, snake case use path)
    pub_use_mappings: Vec<(String, Vec<String>)>,
    private_use_mappings: Vec<(String, Vec<String>)>,
    /// Same format as use mapping but has absolute module path
    /// (snake case item name, snake case absolute module path)
    resolved_mappings: Vec<(String, Vec<String>)>,
    /// For recording information about the return type of fns
    ///
    /// TODO what if the fn is just imported from another module?
    ///
    /// We need:
    ///
    /// 1. A list of mappings from fn name to crate-global identifiers for all the fns available in the module (including imported ones), eg their absolute path or it's deduplicated name
    ///
    /// 2. A list of *all* fns in the crate
    ///
    /// Not easy to know which fns are available in module since some might be called like some_module::my_func() so we have to look at all Expr::Path in the code, not just use statements
    ///
    /// For now only lookup fns which are defined in module (including scopes)
    ///
    /// Should bear in mind how this might support generic associated fns eg T::default() - should be easy to store the extra info needed about the fn, or just store the whole ItemFn
    ///
    /// Module paths work as a unique key for top level items, but for fns in scopes need to just store them by name in a stack. The stack will need to be dynamic and follow the scopes, and pop stuff, so needs to happen during parsing, whereas top level fns get stored in the first pass ie extract_data()
    ///
    ///
    /// Top-level defined fns (possibly from other modules) available in this module, including cases like `use some_module; some_module::some_fn()`
    ///
    /// (<name>, <module path>)
    // fn_info: Vec<(String, Vec<String>)>,
    fn_info: Vec<FnInfo>,
    item_definitons: Vec<ItemDefinition>,
    /// (name, type, syn const)
    consts: Vec<ConstDef>,
    trait_definitons: Vec<RustTraitDefinition>,

    // We need this for extract_data_populate_item_definitions which happens after the modules ModuleData has been created by extract_data, but is populating the `ItemDefiitions` etc, and needs access to the original items in the module for this
    items: Vec<Item>,
}
impl ModuleData {
    fn item_defined_in_module(&self, use_private: bool, item: &String) -> bool {
        let mut definitions = self.pub_definitions.iter();
        if use_private {
            definitions
                .chain(self.private_definitions.iter())
                .any(|definition| definition == item)
        } else {
            definitions.any(|definition| definition == item)
        }
    }
    fn path_starts_with_sub_module(&self, use_private: bool, item: &String) -> bool {
        let mut submodules = self.pub_submodules.iter();
        if use_private {
            submodules
                .chain(self.private_submodules.iter())
                .any(|submodule_name| submodule_name == item)
        } else {
            submodules.any(|submodule_name| submodule_name == item)
        }
    }
}

#[derive(Debug, Clone)]
struct Duplicate {
    namespace: Vec<String>,
    module_path: Vec<String>,
    name: String,
    original_module_path: Vec<String>,
}

/// Types are ultimately needed for:
/// * We can properly cast eg Json::stringify() to JSON.stringify(). I think it most cases we can correctly handle these cases and differentiate from user types by looking up the import, but if the user does something like `let other = Json; other::stringify_with_args()` (not actually valid, can't assign a struct to a var, but can't think of any proper examples right now) or something, we need to know the type of other to know if it's methods need renaming???
/// * calling associated functions on a generic. generics are just types so can't have . methods, but they can have :: asociated fns. We need to know the concrete/runtime type of the generic to know which type to use the `Foo` in `Foo::bar()`.
/// * When we have something that will transpile to a JS primitive so that we know:
///     * it is safe to use operators eg ===, +, etc in place of method equivalents eg .eq(), .add(), etc
///     * `mut`s and `&mut`s must be wrapped in an object
/// * How to deal with dereferencing (ie need to know if we have &mut or & ???)
/// * We can ignore `.clone()` on strings
/// * When a `Copy` item is copied, that will transpiled to an object or array in JS, we must add a `.copy()` or `.clone()` or something
/// * Able to error when attempting to transpile an unimplemented/unsupport method on eg `Option`.
/// * Allows only importing minimum required prelude. The full prelude will be large given the large amount of methods on Vec, Option, etc, so we do want to only import what is actually used, and to know what is actually used we can't just look for use of eg Vec, Some, we also need to look for eg `.map()` but is that map for a Vec, Option, or user defined? we can only know if we know the type of what it is being called on.
/// * `let num = 5;` (or more likely getting a number from JSON) doesn't tell you the type so we need to know what it is eventually inferred as to know whether to use bigint
///
/// "This also made me realise we might want to keep different number types in the transpiled JS, eg i32, f64, as they would have different methods?" I think this just means we need to use `someI32Method(5)` rather than `5.someI32Method()`
///
/// Types are therefore needed to:
///
/// Why not use `syn::Type`? No reason I think. It don't have variants for builtins like i32, but we probably will end up copying most of the variants to handle more complex type with bounds like `impl T + W` etc. Can't work out where is stores the generic for eg `Foo<Bar>`, the Path variant seems to just be a path... TODO debug to test this
///
/// I don't see how any of this requires a Enum or Struct variant/type?
///
/// Generics are probably the hardest part because they don't get defined till later, but by the time we are actually eg calling methods on the type, we should know the generic?
///
/// Do we need separate types for Option and Result?
///
// #[derive(Debug, Clone)]
// enum RustType {
//     // JustForReference(Type),
//     /// For cases/expressions we know cannot return a type, eg `break`
//     NotAllowed,
//     /// Can't be known at this point in analysis, eg the type is inferred somewhere else in the code
//     Unknown,
//     /// Needs implementing
//     Todo,
//     /// ()
//     Unit,
//     /// !
//     Never,
//     /// Shouldn't be used, just used to represent some like the `ToString` in `T: ToString` which should subsequently get ignored and eventually just returned as `RustType::Generic("T")`. What????
//     /// Fns might return impl FooTrait, and that will also be the type of eg any var that the result is assigned to. It's fine not knowing the exact type because you can only call the trait's methods on it, but need to be able to look up the trait's methods to know what type they return.
//     ImplTrait(Vec<InstanceTypeImplTrait>),
//     /// I think this is useful for eg within a fn with generic T and we have vars that have type T
//     /// (name, type)
//     TypeParam(RustTypeParam),
//     /// name
//     TypeParamSimple(String),
//     I32,
//     F32,
//     Bool,
//     String,
//     /// (generic)
//     Option(Box<RustType>),
//     /// (generic)
//     Result(Box<RustType>),
//     /// Need to remember that because StructOrEnum can have generics that have been resolved to concrete types, it means we are using RustType to record the type of both the single defined item that gets recorded in scopes modules/structs and enums, but also instances of the item, stored the in variables vec of the scope
//     Struct(StructOrEnumInstance),
//     Enum(StructOrEnumInstance),
//     Vec(Box<RustType>),
//     Array(Box<RustType>),
//     Tuple(Vec<RustType>),
//     /// (&mut T)
//     MutRef(Box<RustType>),
//     /// (& T) useful to track & as well as &mut so we know what * is operating on??
//     Ref(Box<RustType>),
//     /// (return type)
//     Fn(Box<RustType>),
// }

// Why do we need this? How is it different to RustType?
// MemberType is for types in item definitions and impls, RustType is for instance types
// One reason to have both is that it makes it easier to reason about, ie are we dealing with a definition or an instance
// MemberType `::ParentItem` because... we need some way of signaling it is the same type as the parent type, I guess we could just store the name?
// Most importantly we need to distinguish between whether eg `self` is returned or a new Foo, because in the former case we want to keep any type params that have been made concrete, and in the latter we want a fresh type? this is different from *Self* and Foo, and so named ParentItem instead of self. If we used the body to determine the return type, it would be easy.
// The main difference is that RustType Can be ::NotKnownYet, but this is actually very rare, numbers are the only case I can think of ie `let five = 5;`
// All this is why we can't just have a RustType::String(String), and need to store all the generics on it - exactly only the generics, don't need to store members etc?
// For MemberType, type parameters *cannot be resolved*, for RustType type parameters can be resolved to concrete types
//
// We separate out types like eg ::Vec because we want to know if the type is a js built in so we can convert [].push(x) to push([], x)
//
// Do also need to consider stuff like `impl X for Vec<T>` and `impl X for (i32, i32)`, ie we don't have a *user* type (ie struct/enum) to attach "members" to, so instead need to store a list of ItemType's that have members attached so when we have eg `(3, 3).my_foo()` we can lookup the return type of `.my_foo()` (and also know when to compile to eg myFoo([3, 3]))
//
// So ItemType is effectively used to point to the other types that appear in the item definiton.
//
// It makes sense to just use one of ItemType/InstanceType because they are practically the same and type instances will need to look up impls that match their type
// Also, for matching, Foo<i32> will need to match Foo<T>, etc so it is not as easy as doing `x == y`
#[derive(Debug, Clone, PartialEq, Eq)]
enum RustType {
    /// For cases/expressions we know cannot return a type, eg `break`
    NotAllowed,
    /// Can't be known at this point in analysis, eg the type is inferred somewhere else in the code
    Unknown,
    /// Needs implementing
    Todo,
    // Self_,
    /// I think ParentItem means it is actually `self` not just `Self`???
    ParentItem,
    /// ()
    Unit,
    /// !
    Never,
    /// Fns might return impl FooTrait, and that will also be the type of eg any var that the result is assigned to. It's fine not knowing the exact type because you can only call the trait's methods on it, but need to be able to look up the trait's methods to know what type they return.
    ///
    /// Vec<(Option<module path> (None if a scoped trait), trait (eg FooTrait or Fn(i32) -> i32))>
    ImplTrait(Vec<(Option<Vec<String>>, RustTypeImplTrait)>),
    /// Why does RustTypeParam need to hold resolved values, surely when the param is resolved we just use that type directly? In some cases, eg a fn that returns T, if T is resolved we can just return the resolved type when the fn is called. Other times it might be that the type is resolved, but we need to know later down the line which param was resolved so we can resolve the param where it is used in other places??? Possible but need examples to justify it.
    TypeParam(RustTypeParam),
    // TODO does TypeParam need to store information about the bounds?
    // TODO surely a struct and impl block can have multiple type params with the same name? So we need to keep track of where the param was defined to know which param eg a method arg's type should update
    // TypeParam(String),
    /// name
    // TypeParamSimple(String),
    I32,
    F32,
    Bool,
    String,
    /// (generic)
    Option(Box<RustType>),
    /// (generic)
    Result(Box<RustType>),
    /// Need to remember that because StructOrEnum can have generics that have been resolved to concrete types, it means we are using RustType to record the type of both the single defined item that gets recorded in scopes modules/structs and enums, but also instances of the item, stored the in variables vec of the scope
    // Struct(StructOrEnumItemDefinition),
    // Enum(StructOrEnumInstance),
    // Don't need to store all info about the type, because it should already be stored in the global scope as module data or scoped items. Just need to store a path to that item? Remember we are going to have to resovle paths to items any, ie if we find a `Foo` path we will need to follow the use stmts to find the module it is defined in. For ItemTypes yes, for InstanceTypes I think also yes but we need to all store the resolved state of the generics for each nested type
    // I think we do need to copy the data into the InstanceType because for scoped items we might have a Foo which then gets defined again in a lower scope so if we just look it up to get member info we will find the wrong one and have nothing to pin it to differentiate with like module paths. Yes but we could also give the item definitions indexes or unique ids and then we only have to store those with the InstanceType... NOTE in an item definition, while that items type params won't be resolved, the other types it uses in it's definition might be eg `stuct Foo { bar: Bar<i32> }`
    ///
    /// TODO storing a module path doesn't make much sense if the struct/enum/fn is scoped? Could use an Option which is None if the item is scoped? For now just store the Vec as whatever the current module is (even though this could be confusing for a scoped item), because it doesn't really matter since we always look for scoped items first, and determining whether eg handle_item_fn is for a module level fn or scoped fn would require passing extra args... NO actually we need to know whether we are top level or in a scope because currently we are putting all fns handled with handle_item_fn into the current scope, even if they are top level... which of course should just be scope=0, but this is not a nice approach
    /// TODO IMPORTANT we can't use the paths to definitions approach anyway because instances can exist in parent scopes of the item definition's scope. The best approach seems to be to simply store the item definition (or a reference to it) on the RustType as this seems to be how Rust itself models where/how item are allowed to be used/instantiated. eg:
    /// ```rust
    /// struct AmIHoisted {
    ///     ohno: String,
    /// }
    /// let cool = {
    ///     struct AmIHoisted {
    ///         ohno: i32,
    ///     }
    ///     let am_i_hoisted = AmIHoisted { ohno: 5 };
    ///     am_i_hoisted
    /// };
    /// assert!(cool.ohno == 5);
    /// ```
    /// Alternatively, this wouldn't be a problem if we hoisted *all* scoped definitions to the module level.
    ///
    /// (type params, module path, name)
    StructOrEnum(Vec<RustTypeParam>, Option<Vec<String>>, String),
    // Struct(Vec<RustTypeParam>, Vec<String>, String),
    /// (type params, module path, name)  
    // Enum(Vec<RustTypeParam>, Vec<String>, String),
    // TODO Should we use the same type for both Arrays and Vecs, because they get transpiled to the same thing anyway? NO because we need to handle the types differently, ie arrays need `.copy()` adding when they are moved (although this won't be necessary if the previous value is not used after the move/copy, but this would be hard to determine so need to just always add copy).
    Vec(Box<RustType>),
    Array(Box<RustType>),
    Tuple(Vec<RustType>),
    /// ie `type FooInt = Foo<i32>;`
    /// (name, type)
    UserType(String, Box<RustType>),
    /// (&mut T)
    MutRef(Box<RustType>),
    /// (& T) useful to track & as well as &mut so we know what * is operating on?? NO I think it doesn't matter in practice, we can just check if we have a `&mut` expr and if not just ignore the *
    Ref(Box<RustType>),
    /// (type params, return type)
    // Fn(Vec<RustTypeParam>, Box<RustType>),
    /// fn might be an associated fn in which case first arg will be Some() containing the (possibly resolved) generics of the impl target/self type. Possibly want to also record which type params are defined on the impl block, but see if we can get away without it initially given any impl block type params pretty much have to appear in the target/self type.
    /// (item type params, type params, module path, name)
    Fn(
        Option<Vec<RustTypeParam>>,
        Vec<RustTypeParam>,
        Option<Vec<String>>,
        // TODO arguably it would be better to just store the path and item name all in one, and when looking up the item/fn we are able to determine at that point whether the final one or two elements of the path are a item or associated fn or whatever
        RustTypeFnType,
    ),
    /// We need a separate type for closures because there is no definition with a path/ident to look up like RustType::Fn. Maybe another reason to store the type info directly and avoid using lookups so we don't need two separate variants.
    /// (return type)
    Closure(Box<RustType>),
}
impl RustType {
    fn is_js_primative(&self) -> bool {
        match self {
            RustType::NotAllowed => todo!(),
            RustType::Unknown => todo!(),
            RustType::Todo => todo!(),
            RustType::ParentItem => todo!(),
            RustType::Unit => todo!(),
            RustType::Never => todo!(),
            RustType::ImplTrait(_) => todo!(),
            RustType::TypeParam(_) => todo!(),
            RustType::I32 | RustType::F32 | RustType::Bool | RustType::String => true,
            RustType::Option(_) => todo!(),
            RustType::Result(_) => todo!(),
            RustType::StructOrEnum(_, _, _) => false,
            RustType::Vec(_) => todo!(),
            RustType::Array(_) => false,
            RustType::Tuple(_) => todo!(),
            RustType::UserType(_, _) => todo!(),
            RustType::MutRef(_) => false,
            RustType::Ref(_) => todo!(),
            RustType::Fn(_, _, _, _) => false,
            RustType::Closure(_) => todo!(),
        }
    }
    fn is_mut_ref_of_js_primative(&self) -> bool {
        match self {
            RustType::NotAllowed => todo!(),
            RustType::Unknown => todo!(),
            RustType::Todo => todo!(),
            RustType::ParentItem => todo!(),
            RustType::Unit => todo!(),
            RustType::Never => todo!(),
            RustType::ImplTrait(_) => todo!(),
            RustType::TypeParam(_) => todo!(),
            RustType::I32 => false,
            RustType::F32 => false,
            RustType::Bool => false,
            RustType::String => false,
            RustType::Option(_) => todo!(),
            RustType::Result(_) => todo!(),
            RustType::StructOrEnum(_, _, _) => false,
            RustType::Vec(_) => todo!(),
            RustType::Array(_) => todo!(),
            RustType::Tuple(_) => todo!(),
            RustType::UserType(_, _) => todo!(),
            RustType::MutRef(inner) => inner.is_js_primative(),
            RustType::Ref(_) => todo!(),
            RustType::Fn(_, _, _, _) => todo!(),
            RustType::Closure(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RustTypeFnType {
    /// (fn name)
    Standalone(String),
    /// (item name, fn name)
    AssociatedFn(String, String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RustTypeImplTrait {
    SimpleTrait(String),
    /// (return type)
    Fn(RustType),
}

// Do we want to also store the trait bounds of each type param? This way if we have an unresolved type param that calls some function, we will know what trait to look up to find it. In some cases this might also remove the need for looking forward to resolve type params, if all we need to do with the type param/value/intance/type is call a method on it.
#[derive(Debug, Clone, PartialEq, Eq)]
struct RustTypeParam {
    name: String,
    type_: RustTypeParamValue,
}
// impl RustTypeGeneric

#[derive(Debug, Clone, PartialEq, Eq)]
enum RustTypeParamValue {
    /// Can't be known at this point in analysis, eg the type is inferred somewhere else in the code
    Unresolved,
    RustType(Box<RustType>),
}

#[derive(Debug, Clone)]
struct ScopedVar {
    name: String,
    mut_: bool,
    // TODO don't need this because it is part of the type and is record by the `type_` field
    // mut_ref: bool,
    // TODO
    type_: RustType,
}
impl ScopedVar {
    fn is_mut_ref(&self) -> bool {
        match self.type_ {
            RustType::MutRef(_) => true,
            _ => false,
        }
    }
}

// #[derive(Debug, Clone)]
// struct EnumInfo {
//     ident: String,
//     methods: Vec<ImplItemTemp>,
// }

// #[derive(Debug, Clone)]
// struct StructOrEnumMethod {
//     item_name: String,
//     return_type: RustType,
// }

// #[derive(Debug, Clone)]
// enum StructOrEnumSynObject {
//     Struct(ItemStruct),
//     Enum(ItemEnum),
// }

// #[derive(Debug, Clone)]
// enum MemberInfoFieldOrVariant {
//     Field,
//     Variant,
// }

#[derive(Debug, Clone)]
enum StructFieldInfo {
    UnitStruct,
    TupleStruct(Vec<RustType>),
    RegularStruct(Vec<(String, RustType)>),
}

#[derive(Debug, Clone)]
struct StructDefinitionInfo {
    fields: StructFieldInfo,
    syn_object: Option<ItemStruct>,
}

#[derive(Debug, Clone)]
enum EnumVariantInputsInfo {
    Named {
        ident: String,
        input_type: RustType,
    },
    /// (input type)
    Unnamed(RustType),
}

#[derive(Debug, Clone)]
struct EnumVariantInfo {
    ident: String,
    inputs: Vec<EnumVariantInputsInfo>,
}

#[derive(Debug, Clone)]
struct EnumDefinitionInfo {
    members: Vec<EnumVariantInfo>,
    syn_object: ItemEnum,
}

#[derive(Debug, Clone)]
enum StructOrEnumDefitionInfo {
    Struct(StructDefinitionInfo),
    Enum(EnumDefinitionInfo),
}

/// Similar to StructOrEnum which gets used in RustType, but is for storing info about the actual item definition, rather than instances of, so eg we don't need to be able to store resolved generics. Minor differences but making distinct type helps with reasoning about the different use cases.
/// Just structs and enums or should we include functions?
#[derive(Debug, Clone)]
struct ItemDefinition {
    ident: String,
    is_copy: bool,
    // /// Fields and enum variants. Methods etc are stored in impl blocks?
    // members: Vec<StructFieldInfo>,
    // members: Vec<ImplItem>,
    // TODO do we need to know eg bounds for each generic?
    generics: Vec<String>,
    // syn_object: StructOrEnumSynObject,
    struct_or_enum_info: StructOrEnumDefitionInfo,
}

// We have some kind of usage of the struct/enum, eg `let foo = Foo::Bar(5)` and want to check if the struct/enum is/has generic(s) and if so is eg the input to variant `Bar` one of those generic(s). For now just store the whole ItemStruct/ItemEnum and do the checking each time from wherever eg `Foo::Bar(5)` is.
// generic could be determined by: input for enum variant, input to method, field value of struct constructor.
// #[derive(Debug, Clone)]
// struct ItemDefinition {
//     ident: String,
//     /// so this includes methods, fields, enum variants, etc, basically anything that can come after . or :: so we will usually get a different type from the "parent"
//     ///
//     /// When look up return type of a member and the return type is Self, then need to return this StructOrEnum, as a RustType, possibly first updating the generics based on eg the types used in the args of the assignment variable's type.
//     ///
//     /// NOTE we might have nested generics so need to be careful to keep them separate and not flatten them so they can be resolved later?
//     /// like `let var = Foo<Bar<Baz<Option(i32)>>> = Foo::new()`?????
//     /// Don't need to store member info on an instance?
//     // members: Vec<MemberInfo>,
//     // members: Vec<ImplItem>,
//     generics: Vec<RustTypeParam>,
//     syn_object: StructOrEnumSynObject,
// }
impl ItemDefinition {
    /// Update generics based on types of args
    ///
    /// For all the generics of the struct/enum...
    /// ...for enum check if any of the arguments to any of the variants are the generic type...
    /// ...(if) we found a generic so now we need to find the type of the argument being passed and we will have the full `MyEnum<FoundGeneric>` type
    fn attempt_to_resolve_generics(
        &self,
        field_or_variant_name: &String,
        args: &Vec<(JsExpr, RustType)>,
    ) -> Vec<RustTypeParam> {
        let mut possibly_resolved_generics = Vec::new();
        for generic in &self.generics {
            match &self.struct_or_enum_info {
                StructOrEnumDefitionInfo::Struct(_) => todo!(),
                StructOrEnumDefitionInfo::Enum(enum_def_info) => {
                    let item_enum = &enum_def_info.syn_object;
                    // ...for enum check if any of the arguments to any of the variants are the generic type...
                    for v in &item_enum.variants {
                        if v.ident == field_or_variant_name {
                            match &v.fields {
                                Fields::Named(fields_named) => {
                                    for (i, field) in fields_named.named.iter().enumerate() {
                                        match &field.ty {
                                            Type::Path(type_path) => {
                                                if type_path.path.segments.first().unwrap().ident
                                                    == generic
                                                {
                                                    // ...we found a generic so now we need to find the type of the argument being passed and we will have the full `MyEnum<FoundGeneric>` type
                                                    let (_js_expr, rust_type) = args[i].clone();
                                                    possibly_resolved_generics.push(
                                                        RustTypeParam {
                                                            name: generic.clone(),
                                                            type_: RustTypeParamValue::RustType(
                                                                Box::new(rust_type),
                                                            ),
                                                        },
                                                    );
                                                    continue;
                                                }
                                            }
                                            Type::Verbatim(_) => todo!(),
                                            _ => todo!(),
                                        }
                                    }
                                }
                                Fields::Unnamed(fields_unnamed) => {
                                    for (i, field) in fields_unnamed.unnamed.iter().enumerate() {
                                        match &field.ty {
                                            Type::Path(type_path) => {
                                                if type_path.path.segments.first().unwrap().ident
                                                    == generic
                                                {
                                                    // ...we found a generic so now we need to find the type of the argument being passed and we will have the full `MyEnum<FoundGeneric>` type
                                                    let (_js_expr, rust_type) = args[i].clone();
                                                    possibly_resolved_generics.push(
                                                        RustTypeParam {
                                                            name: generic.clone(),
                                                            type_: RustTypeParamValue::RustType(
                                                                Box::new(rust_type),
                                                            ),
                                                        },
                                                    );
                                                    continue;
                                                }
                                            }
                                            Type::Verbatim(_) => todo!(),
                                            _ => todo!(),
                                        }
                                    }
                                }
                                Fields::Unit => todo!(),
                            }
                        }
                    }
                }
            }
            possibly_resolved_generics.push(RustTypeParam {
                name: generic.clone(),
                type_: RustTypeParamValue::Unresolved,
            });
        }
        possibly_resolved_generics
    }
}

#[derive(Debug, Clone)]
enum ItemDefinitions {
    Struct,
    Enum,
    Fn,
}

/// Similar to ImplItemTemp but with less info (might turn out that we need to add more) because we are only using it for scoped items
// #[derive(Debug, Clone)]
// struct MethodInfo {
//     method_ident: String,
//     item_stmt: JsImplItem,
//     return_type: RustType,
// }

#[derive(Debug, Clone)]
struct RustGeneric {
    ident: String,
    // (module path, trait name)
    trait_bounds: Vec<(Option<Vec<String>>, String)>,
}

#[derive(Debug, Clone)]
struct RustImplBlock {
    generics: Vec<RustGeneric>,
    trait_: Option<(Option<Vec<String>>, String)>,
    // Note this can a generic param
    target: RustType,
    items: Vec<RustImplItem>,
}

// TODO clean up these types since eg there is duplication of the fn ident
#[derive(Debug, Clone)]
struct RustImplItem {
    ident: String,
    item: RustImplItemItem,
    // return_type: RustType,
    syn_object: ImplItem,
}
#[derive(Debug, Clone)]
enum RustImplItemItem {
    /// (private, static, fn info, js fn),
    Fn(bool, bool, FnInfo, JsFn),
    Const(JsLocal),
}

#[derive(Debug, Clone)]
struct ConstDef {
    name: String,
    type_: RustType,
    syn_object: ItemConst,
}

/// Not just for methods, can also be an enum variant with no inputs
#[derive(Debug, Clone)]
struct FnInfo {
    // TODO No point storing all the info like inputs and return types separately, as these need to be stored on RustType::Fn anyway for eg closures where we won't be storing a fn info?? Keep both for now and revisit later. Note fns idents can just appear in the code and be called whereas a closure will be a var which already has a type.
    ident: String,
    /// Does this include receiver/self types? NO in handle_item_fn we are filtering out any self type. Could just store it as RustType::Self, but seems pointless if we don't actually need it for anything
    inputs_types: Vec<RustType>,
    generics: Vec<String>,
    // NO! for methods we want to store the actual fn type. fns can be assigned to vars, and we want to be able to pass the Path part of the fn, and *then* call it and determine the return type
    return_type: RustType,
    // /// type of fn eg Fn(i32) -> ()
    // rust_type: RustType,
    // TODO optionally add enum for Field, AssociatedFn, Method, etc
}
impl FnInfo {
    fn attempt_to_resolve_type_params_using_arg_types(
        &self,
        args: &Vec<RustType>,
    ) -> Vec<RustTypeParam> {
        self.generics
            .iter()
            .map(|g| {
                let matched_arg_rust_type =
                    self.inputs_types
                        .iter()
                        .enumerate()
                        .find_map(|(i, input_type)| {
                            match input_type {
                                RustType::TypeParam(type_param) if g == &type_param.name => {
                                    Some(args[i].clone())
                                }
                                // TODO what about types that *contain* a type param eg `foo: Option<T>`
                                _ => None,
                            }
                        });

                let rust_type_param_value =
                    if let Some(matched_arg_rust_type) = matched_arg_rust_type {
                        RustTypeParamValue::RustType(Box::new(matched_arg_rust_type))
                    } else {
                        RustTypeParamValue::Unresolved
                    };

                RustTypeParam {
                    name: g.clone(),
                    type_: rust_type_param_value,
                }
            })
            .collect::<Vec<_>>()
    }
}

// #[derive(Debug, Clone)]
// struct MyGeneric {
//     name: String,
//     type_: RustType,
// }

// #[derive(Debug, Clone)]
// pub enum VarOrFnInfo {
//     ScopedVar(ScopedVar),
//     FnInfo(FnInfo)
// }

/// variable: Vec<ScopedVar>,
/// fns: Vec<FnInfo>,
/// generics: Vec<MyGeneric>,
/// structs_enums: Vec<StructOrEnumMethods>,
#[derive(Debug, Default, Clone)]
struct GlobalDataScope {
    variables: Vec<ScopedVar>,
    fns: Vec<FnInfo>,
    /// Why does a scope have generics?? for fns/methods?
    // generics: Vec<MyGeneric>,
    generics: Vec<RustTypeParam>,
    // Need to keep track of where the generic is used, eg input to enum variant, input to method, result of some fn call in the body, etc so that when eg we have Foo::Bar(T) getting instantiated with `let foo = Foo::Bar(5)`, we know to then update the type of T to be i32
    item_definitons: Vec<ItemDefinition>,
    /// TODO I think we want to hoist all scoped impl blocks to the module level, ie get them during the first pass? Well traits can't be hoisted because traits can shadow the names of existing traits in parents scopes, but if we try to impl a shadowed trait you get a "multiple applicable items in scope" (I think this just means you need an explicit pah like <MyType as Trait1> or something ???) error so I think hoisting impls is fine, and potentially necessary because you can use an impl'd method in a parent scope of the impl or even a different scope branch, ie rustc seems to hoist impl blocks, the only limitation is that the any impl'd trait and the target type are (of course) in scope of the impl block. NO - can't hoist `impl MyTrait` blocks because if there is multiple scoped `MyTrait`s (ie duplicate names), we won't know which `impl MyTrait` to use.
    ///
    ///
    /// I think the solution is as follows. First we need to consider normal impls separately to trait impls.
    /// 1. Normal impls can be scoped and duplicated if the type they are implementing is duplicated/shadowed, so can't just hoist all of them to module level, so the impl blocks need to be scoped but we do need to hoist them to the same scope as the type definition, give the methods/items implemented can be used before the impl and in parent scopes
    /// 2. Trait impls are different. For normal impls we cannot apply say `impl Foo { fn foo() {} }` multiple times for the same item/type, whereas for trait impls we can have duplicate, identical impls say `impl Bar for Foo { fn foo() {} }`, as long as we also have multiple `Bar`s, ie the trait is shadowed/duplicated. Also, we cannot call any methods unless the trait for which the method was implemented is in scope, so in this case we want to hoist the impl block to *either* the same scope as the trait or the same scope as the (concrete type implemented)
    /// 3. For trait impls of generic types...
    ///
    ///
    /// Also need to consider scoped use stmts, which I don't know if the first pass handles?
    ///
    /// If we tried to premptively add all impl items for each type, we would have to differentiate between say Foo<i32> and Foo<f32>, and so we would have to add records for every possible concrete type param. For this reason it is better to just store the impl blocks as is, with the trait and generic info eg `impl MyTrait for T` and then work out which impl blocks apply to which type/method as and when needed, ie we are looking up info on a method.
    /// (ident, type)
    // impl_blocks: Vec<(RustType, Vec<RustImplItem>)>,
    // I think it is easier to just store the syn object because we need so much info, and I don't think we will be creating impl blocks manually so don't *need* our own type
    // Also need to consider (possibly multiple) impls being defined in different modules to the target type
    // impl_blocks: Vec<ItemImpl>,
    impl_blocks: Vec<RustImplBlock>,
    // trait_definitons: Vec<RustTypeImplTrait>,
    trait_definitons: Vec<RustTraitDefinition>,
    consts: Vec<ConstDef>,
    /// Blocks, match arms, closures, etc are differnt to fn scopes because they can access variables from their outer scope. However, they are similar in that you loose all the items and variables (not impls though) defined in them, at the end of their scope. This is a flag to indicate this type of scope and thus when looking for things such as variables, we should also look in the surrounding scope.
    look_in_outer_scope: bool,
    use_mappings: Vec<(String, Vec<String>)>,
}

#[derive(Debug, Clone)]
struct RustTraitDefinition {
    name: String,
    // impl_items:
}

#[derive(Debug, Clone)]
struct GlobalData {
    crate_path: Option<PathBuf>,
    modules: Vec<ModuleData>,
    crates: Vec<CrateData>,
    // TODO doesn't handle capturing scopes which needs rules to mimic how a closure decides to take &, &mut, or ownership
    // NOTE use separate Vecs for vars and fns because not all scopes (for vars) eg blocks are fns
    // NOTE don't want to pop fn after we finish parsing it because it will be called later in the same scope in which it was defined (but also might be called inside itself - recursively), so only want to pop it once it's parent scope completes, so may as well share scoping with vars
    // NOTE need to store vars and fns in the same Vec to ensure we know the precendence in cases like `fn foo() {}; fn bar() {}; let foo = bar;` NO - functions are hoisted so we always want to check if a var with that ident exists first *then* look for a fn, first in the scopes, then at the module level
    scopes: Vec<GlobalDataScope>,
    // TODO combine this with impl_items
    // struct_or_enum_methods: Vec<StructOrEnumMethods>,
    // scopes: Vec<Vec<ScopedVar>>,
    /// (the purpose originally was for self not Self... which is not needed, but Self is neccessary) the purpose of this is for storing the type of `Self` *not* `self`, eg if a impl fn body contains `let foo: Self = Self {};`, we will want to know what Self is so we know the types of `foo.some_field` etc
    ///
    /// We have a Vec in case there is an impl block nested inside an impl block?
    impl_block_target_type: Vec<RustType>,
    // TODO handle closures - which don't have explicitly specified return type, need to infer it from return value
    // scoped_fns: Vec<ItemFn>,
    rust_prelude_types: RustPreludeTypes,
    rust_prelude_definitions: Vec<(String, ItemDefinition)>,
    /// (trait name, impl item)
    default_trait_impls: Vec<(String, JsImplItem)>,
    /// Used for working out which `default_trait_impls` elements to add to which classes
    ///
    /// (class name, trait name)
    default_trait_impls_class_mapping: Vec<(String, String)>,
    /// For temporary storage of JS methods prior to adding to JS classes
    /// TODO doesn't seem like we are actually populating this even though it has been used for a while?
    // impl_items_for_js: Vec<ImplItemTemp>,
    /// For looking up return types of methods etc
    // impl_items_for_types: Vec<(RustType, RustImplItem)>,
    // We keep the impl blocks at the crate level rather than in the relevant Module because different it is not possible to impl the same eg method name on the same struct, even using impl blocks in completely separate modules. Impl item idents must be unique for a given type across the entire crate. This is because impl'd items are available on the item definition/instance they are targetting, not only in parent scopes, but also parent modules.
    // impl_blocks: Vec<ItemImpl>,
    impl_blocks: Vec<RustImplBlock>,
    duplicates: Vec<Duplicate>,
    transpiled_modules: Vec<JsModule>,
    // /// For keeping track of whether we are parsing items at the module level or in a fn scope, so that we know whether we need to add the items to `.scopes` or not.
    // at_module_top_level: bool,
}
impl GlobalData {
    fn new(crate_path: Option<PathBuf>, duplicates: Vec<Duplicate>) -> GlobalData {
        // Create prelude definitions
        let i32_def = ItemDefinition {
            ident: "i32".to_string(),
            // TODO even though i32 is Copy, we are only interested in *structs* which are Copy, though this might be wrong if is_copy is used for other purposes.
            is_copy: false,
            generics: Vec::new(),
            struct_or_enum_info: StructOrEnumDefitionInfo::Struct(StructDefinitionInfo {
                fields: StructFieldInfo::RegularStruct(Vec::new()),
                syn_object: None,
            }),
        };
        // TODO should the ident be `String` or `str`???
        let string_def = ItemDefinition {
            ident: "String".to_string(),
            is_copy: false,
            generics: Vec::new(),
            struct_or_enum_info: StructOrEnumDefitionInfo::Struct(StructDefinitionInfo {
                fields: StructFieldInfo::RegularStruct(Vec::new()),
                syn_object: None,
            }),
        };
        let str_def = ItemDefinition {
            ident: "str".to_string(),
            is_copy: false,
            generics: Vec::new(),
            struct_or_enum_info: StructOrEnumDefitionInfo::Struct(StructDefinitionInfo {
                fields: StructFieldInfo::RegularStruct(Vec::new()),
                syn_object: None,
            }),
        };

        // let ravascript_prelude_crate = CrateData {
        //     name: "ravascript".to_string(),
        // };

        GlobalData {
            crate_path,
            modules: Vec::new(),
            // crates: vec![ravascript_prelude_crate],
            crates: vec![],
            // init with an empty scope to ensure `scopes.last()` always returns something TODO improve this
            scopes: vec![GlobalDataScope::default()],
            // struct_or_enum_methods: Vec::new(),
            impl_block_target_type: Vec::new(),
            // scoped_fns: vec![],
            rust_prelude_types: RustPreludeTypes::default(),
            default_trait_impls_class_mapping: Vec::new(),
            rust_prelude_definitions: vec![
                ("i32".to_string(), i32_def),
                ("String".to_string(), string_def),
                ("str".to_string(), str_def),
            ],
            default_trait_impls: Vec::new(),
            // impl_items_for_js: Vec::new(),
            duplicates,
            transpiled_modules: Vec::new(),
            impl_blocks: Vec::new(),
            // at_module_top_level: false,
        }
    }

    // handling paths:
    // A path could be:
    // a var (path.len()=1)
    // an item (ie a struct, enum, or fn)
    // a module (only if path.len()>1 and the final element cannot be a module)
    // We can't just look up the item name in global_data, because multiple modules might have the same item name. So the first thing we want to do is determine the module path. eg resolve self/super/crate, follow any use stmts etc given the path might a single element specificying an item name but that item is actually used from a differnt module. If there is no module path, ie the first element is not self/super/crate/some module, and the item has not been use'd into scope, then look up through the scopes to find it (could be a var (if path.len=1 and only in certain postitions like assignments and args, not eg the type of an input) or a scoped item). I believe we don't have to worry about scoped modules because we look for them during the first pass so they should be included in global_data???
    // Can actually just look up scoped vars/items first??
    //
    // remember we want two thing when lookup up paths:
    // 1. In all cases (I think?), to find the type of whatever it points to
    // 2. In cases where we need to transpile the path, to determine what the transpiled path should be, eg what is the namespaced JS ident for the item.

    /// Needs to be extended to also lookup module level items (and also return namespaced JS path?)
    // fn lookup_scoped_var_or_item_definiton(&self, path: &Vec<String>) -> Option<ItemDefinition> {
    // Need to know eg if a var is mut, so can't just return RustType(??) could mut be added as a RustType??
    // fn lookup_scoped_var_or_item_definiton(&self, path: &Vec<String>) -> Option<RustType> {
    fn lookup_scoped_var_or_item_definiton(&self, path: &Vec<String>) -> Option<VarItemFn> {
        self.scopes.iter().rev().find_map(|scope| {
            // Note variables, and items definitions can't shadow each other in the same scope, so don't need to worry about the order in which each var and item definition was defined in the scope, ie vars and item definitions don't need to be together sorted in one big list, can't just look through the list of vars *then* look through the list of item definitions.
            let first = &path[0];
            let var = scope.variables.iter().find(|v| &v.name == first);
            let func = scope.fns.iter().find(|f| &f.ident == first);
            let se = scope.item_definitons.iter().find(|se| &se.ident == first);
            if path.len() == 1 {
                if let Some(var) = var {
                    return Some(VarItemFn::Var(var.clone()));
                } else if let Some(func) = func {
                    return Some(VarItemFn::Fn(func.clone()));
                } else if let Some(se) = se {
                    return Some(VarItemFn::StructOrEnum(se.clone()));
                } else {
                    return None;
                }
            } else if path.len() == 2 {
                // if let Some(var) = var {
                //     return Some(VarItemFn::Var(var.clone()));
                // } else if let Some(func) = func {
                //     return Some(VarItemFn::Fn(func.clone()));
                // } else if let Some(se) = se {
                //     return Some(VarItemFn::StructOrEnum(se.clone()));
                // } else {
                //     return None;
                // }
                todo!()
            } else {
                todo!()
            }
        })
    }

    fn lookup_fn_definition_known_module(
        &self,
        name: String,
        module_path: &Option<Vec<String>>,
    ) -> FnInfo {
        if let Some(module_path) = module_path {
            let module = self
                .modules
                .iter()
                .find(|m| &m.path == module_path)
                .unwrap();
            module
                .fn_info
                .iter()
                .cloned()
                .find(|se| se.ident == name)
                .unwrap()
        } else {
            self.scopes
                .iter()
                .rev()
                .find_map(|s| s.fns.iter().rev().cloned().find(|se| se.ident == name))
                .unwrap()
        }
    }

    fn lookup_item_definition_known_module(
        &self,
        name: &String,
        module_path: &Option<Vec<String>>,
    ) -> ItemDefinition {
        if let Some(module_path) = module_path {
            let module = self
                .modules
                .iter()
                .find(|m| &m.path == module_path)
                .unwrap();
            module
                .item_definitons
                .iter()
                .cloned()
                .find(|se| &se.ident == name)
                .unwrap()
        } else {
            self.scopes
                .iter()
                .rev()
                .find_map(|s| {
                    s.item_definitons
                        .iter()
                        .rev()
                        .cloned()
                        .find(|se| &se.ident == name)
                })
                .unwrap()
        }
    }

    fn syn_type_to_rust_type_struct_or_enum(
        &self,
        current_module: &Vec<String>,
        // generics: &Vec<RustTypeParam>,
        syn_type: &Type,
    ) -> (Vec<RustTypeParam>, Option<Vec<String>>, String) {
        let type_path = match syn_type {
            Type::Path(type_path) => {
                type_path
                    .path
                    .segments
                    .iter()
                    .map(|seg| {
                        RustPathSegment {
                            ident: seg.ident.to_string(),
                            turbofish: match seg.arguments {
                                PathArguments::None => Vec::new(),
                                // TODO support nested turbofish types
                                PathArguments::AngleBracketed(_) => todo!(),
                                PathArguments::Parenthesized(_) => todo!(),
                            },
                        }
                    })
                    .collect::<Vec<_>>()
            }
            _ => todo!(),
        };

        // // Check if path is a type param
        // if type_path.len() == 1 {
        //     let is_type_param = generics.iter().map(|gen| gen.name).any(|gen_name| gen_name == type_path[0].ident);
        //     if is_type_param {
        //         return
        //     }
        // }

        // TODO should just use get_path to look for scoped item?
        let scoped_item = self.scopes.iter().rev().find_map(|scope| {
            scope
                .item_definitons
                .iter()
                .find(|f| f.ident == type_path[0].ident)
        });
        // TODO can we not use get_path_without_namespacing() for everything?
        let (item_def, module_path, item_path) = if let Some(scoped_item) = scoped_item {
            assert!(type_path.len() == 1);
            (scoped_item.clone(), None, type_path[0].ident.clone())
        } else {
            let (module_path, item_path, is_scoped) = get_path(
                false,
                false,
                true,
                type_path,
                self,
                current_module,
                current_module,
            );
            assert!(item_path.len() == 1);
            // dbg!("yes");
            let item_def = self.lookup_item_def_known_module_assert_not_func(
                &Some(module_path.clone()),
                &item_path[0].ident,
            );
            // dbg!("ytes222");
            (
                item_def,
                Some(module_path.clone()),
                item_path[0].ident.clone(),
            )
        };
        (
            item_def
                .generics
                .iter()
                .map(|gen| RustTypeParam {
                    name: gen.clone(),
                    type_: RustTypeParamValue::Unresolved,
                })
                .collect::<Vec<_>>(),
            module_path,
            item_path,
        )
    }

    fn lookup_item_def_known_module_assert_not_func(
        &self,
        module_path: &Option<Vec<String>>,
        name: &String,
    ) -> ItemDefinition {
        if let Some(module_path) = module_path {
            let module = self
                .modules
                .iter()
                .find(|m| &m.path == module_path)
                .unwrap();

            let func = module.fn_info.iter().find(|se| &se.ident == name);
            assert!(func.is_none());

            module
                .item_definitons
                .iter()
                .find(|se| &se.ident == name)
                .unwrap()
                .clone()
        } else {
            // Look for scoped items
            // dbg!(&self.scopes);
            self.scopes
                .iter()
                .rev()
                .find_map(|scope| {
                    let var = scope.variables.iter().find(|v| &v.name == name);
                    let func = scope.fns.iter().find(|f| &f.ident == name);
                    assert!(var.is_none() && func.is_none());

                    scope.item_definitons.iter().find(|f| &f.ident == name)
                })
                .unwrap()
                .clone()
        }
    }

    // NOTE don't need this because we are already resolving paths with get_path()??
    // This looks up *any ident/Path in a module*, so the item might not actually be defined in the current module and has been use'd. We also need to return the module path of the item, which of course might not be the current module, because this item definition will go into a rust type which will need to look up the item definition
    // Alternatively could just make sure we store all items that are either defined *or* use'd in a module. This should work because module level item names must still be unique even if they are just use'd rather than defined, and currently we aren't yet acutally populating modules.item_definition with anything anyway. TODO come back to this once codebase has been cleaned up and simplified and just handle single module cases for now.
    /// returns None if no item is found
    ///
    /// Option<moule path> will be None for scoped items
    ///
    ///  -> Option<(Option<moule path>, item definition)>
    ///
    /// Used for looking up types eg `fn my_fun(my_arg: some::path::to::a::Type) {}`, not vars
    fn lookup_item_definition_any_module(
        &self,
        current_module_path: &Vec<String>,
        path: &Vec<String>,
        // current_module: &Vec<String>,
    ) -> Option<(Option<Vec<String>>, ItemDefinition)> {
        let first = &path[0];
        // TODO should just use get_path to look for scoped items?
        if let Some(item_def) = self.lookup_scoped_item_definiton(first) {
            return Some((None, item_def));
        } else {
            let (item_module_path, item_path, _is_scoped) = get_path(
                false,
                false,
                true,
                path.iter()
                    .map(|seg| RustPathSegment {
                        ident: seg.clone(),
                        turbofish: Vec::new(),
                    })
                    .collect::<Vec<_>>(),
                self,
                current_module_path,
                current_module_path,
            );

            if item_module_path == vec!["prelude_special_case".to_string()] {
                // Get prelude item definitions
                let (_name, def) = self
                    .rust_prelude_definitions
                    .iter()
                    .find(|(name, def)| name == &item_path[0].ident)
                    .unwrap();
                return Some((Some(item_module_path), def.clone()));
            }

            let item_module = self
                .modules
                .iter()
                .find(|m| &m.path == &item_module_path)
                .unwrap();

            // TODO if the path is eg an associated fn, should we return the item or the fn? ie se or RustType?
            let se = item_module
                .item_definitons
                .iter()
                .find(|se| se.ident == item_path[0].ident);
            if let Some(se) = se {
                return Some((Some(item_module_path), se.clone()));
            } else {
                todo!()
            }

            // let item_definition = self.
            // let module_item_definition = current_module.item_definitons.iter().find(|se| &se.ident == path);
            // if let Some(item_def) = scoped_item_definition {
            //     // todo!();
            //     Some((None, item_def.clone()))
            // } else if let Some(item_def) = module_item_definition {
            //     // todo!();
            //     Some((Some(current_module), item_def.clone()))
            // } else {
            //     None
            // }
        }
    }

    // TODO should also look up fns?
    fn lookup_scoped_item_definiton(&self, name: &String) -> Option<ItemDefinition> {
        self.scopes
            .iter()
            .rev()
            .find_map(|s| s.item_definitons.iter().rev().find(|se| &se.ident == name))
            .cloned()
    }

    fn lookup_trait_definition_any_module(
        &self,
        current_module_path: &Vec<String>,
        path: &Vec<String>,
        // current_module: &Vec<String>,
    ) -> Option<(Option<Vec<String>>, RustTraitDefinition)> {
        let first = &path[0];
        let scoped_trait_def = self
            .scopes
            .iter()
            .rev()
            .find_map(|s| s.trait_definitons.iter().find(|t| &t.name == first));

        // TODO should just use get_path to look for scoped traits?
        if let Some(trait_def) = scoped_trait_def {
            return Some((None, trait_def.clone()));
        } else {
            let (item_module_path, item_path, _is_scoped) = get_path(
                false,
                false,
                true,
                path.iter()
                    .map(|seg| RustPathSegment {
                        ident: seg.clone(),
                        turbofish: Vec::new(),
                    })
                    .collect::<Vec<_>>(),
                self,
                current_module_path,
                current_module_path,
            );
            let item_module = self
                .modules
                .iter()
                .find(|m| &m.path == &item_module_path)
                .unwrap();

            let trait_definiton = item_module
                .trait_definitons
                .iter()
                .find(|t| t.name == item_path[0].ident);
            if let Some(trait_definiton) = trait_definiton {
                return Some((Some(item_module_path), trait_definiton.clone()));
            } else {
                todo!()
            }
        }
    }

    fn get_module_mut(&mut self, module_path: &Vec<String>) -> &mut ModuleData {
        self.modules
            .iter_mut()
            .find(|m| &m.path == module_path)
            .unwrap()
    }

    // This Doesn't/shouldn't look up methods as far as I can tell (methods are always handled directly in handle_expr_method_call) so rename
    // fn lookup_method_or_associated_fn(
    fn lookup_associated_fn(
        &self,
        item_generics: &Vec<RustTypeParam>,
        item_module_path: &Option<Vec<String>>,
        sub_path: &RustPathSegment,
        item_path_seg: &String,
        item_def: &ItemDefinition,
        // ) -> Option<PartialRustType> {
    ) -> Option<RustType> {
        let impl_method = self.lookup_impl_item_item(
            item_generics,
            item_module_path,
            sub_path,
            item_path_seg,
            item_def,
        );
        let impl_method = if let Some(impl_method) = impl_method {
            match impl_method.item {
                RustImplItemItem::Fn(private, static_, fn_info, js_fn) => {
                    // If turbofish exists on fn path segment then use that for type params, otherwise use the unresolved params defined on the fn definition
                    let fn_generics = if sub_path.turbofish.len() > 0 {
                        sub_path
                            .turbofish
                            .iter()
                            .enumerate()
                            .map(|(i, g)| RustTypeParam {
                                name: fn_info.generics[i].clone(),
                                type_: RustTypeParamValue::RustType(Box::new(g.clone())),
                            })
                            .collect::<Vec<_>>()
                    } else {
                        // NOTE for now we are assuming turbofish must exist for generic items, until we implement a solution for getting type params that are resolved later in the code
                        assert!(fn_info.generics.len() == 0);
                        fn_info
                            .generics
                            .iter()
                            .map(|g| RustTypeParam {
                                name: g.clone(),
                                type_: RustTypeParamValue::Unresolved,
                            })
                            .collect::<Vec<_>>()
                    };

                    // Some(PartialRustType::RustType(RustType::Fn(
                    //     Some(item_generics.clone()),
                    //     fn_generics,
                    //     item_module_path.clone(),
                    //     RustTypeFnType::AssociatedFn(item_def.ident, sub_path.ident),
                    // )))
                    Some(RustType::Fn(
                        Some(item_generics.clone()),
                        fn_generics,
                        item_module_path.clone(),
                        RustTypeFnType::AssociatedFn(
                            item_def.ident.clone(),
                            sub_path.ident.clone(),
                        ),
                    ))
                }
                RustImplItemItem::Const(_) => todo!(),
            }
        } else {
            None
        };
        impl_method
    }
    fn lookup_impl_item_item(
        &self,
        item_generics: &Vec<RustTypeParam>,
        item_module_path: &Option<Vec<String>>,
        sub_path: &RustPathSegment,
        item_name: &String,
        item_def: &ItemDefinition,
        // ) -> Option<PartialRustType> {
    ) -> Option<RustImplItem> {
        // For now focus on supporting explicit gnerics ie turbofish etc so don't have to worry about unresolved types, and module level items so I don't have too much about complex scope shadowing behaviours.

        // Look for associated fn of item (struct or enum)

        // Look through all impl blocks which match item to find impl item which matches subpath name

        // First look for method in direct (non-trait) impls
        // TODO for generic structs we need to know the concrete types to know if we should match eg Foo<i32>
        let scoped_impls = self.scopes.iter().map(|s| s.impl_blocks.iter()).flatten();
        let scoped_impl_method =
            self.impl_blocks
                .iter()
                .chain(scoped_impls)
                .find_map(|impl_block| {
                    let types_match = struct_or_enum_types_match(
                        &impl_block.target,
                        &item_generics,
                        &item_module_path,
                        &item_def.ident,
                    );

                    // If we have a matching, non-trait impl block, look to see if it contains the method
                    if types_match && impl_block.trait_.is_none() {
                        impl_block
                            .items
                            .iter()
                            .find(|impl_item| impl_item.ident == sub_path.ident)
                            .cloned()
                    } else {
                        None
                    }
                });

        // Now look for method in trait impls
        // First we need to know which Traits are impl'd for the struct because we need to know it's "trait bounds" to know whether it matches eg:
        // `impl<T: OtherTrait> MyTrait for T {}`
        // Note this could be recursive, ie in the above example our type would impl both OtherTrait and MyTrait, but we need to first determine that it impls OtherTrait, and only then can we determine that it therefore also impls MyTrait. For example
        // impl Trait1 for MyStruct
        // impl<T: Trait1> Trait2 for T
        // impl<T: Trait2> Trait3 for T
        // impl<T: Trait3> Trait4 for T
        // etc
        // To know MyStruct impls Trait3 we have to already know it impls Trait2, which I think means every time we match a trait we must do at least 1 more pass to check if any other traits now match, and repeat this until no news traits are matched.
        // Also note it doesn't work this out upfront eg in the first pass, because for generic structs it can depend on the concrete types of the generics so will need calculating individually on demand at the point we know the concrete type. For non-generic structs however I think we could calculate up front which will be worth doing at some point for better performance.

        // (trait module path (None for scoped), trait name)
        let mut found_traits_previous: Vec<(Option<Vec<String>>, String)> = Vec::new();
        let mut found_traits: Vec<(Option<Vec<String>>, String)> = Vec::new();
        let mut matched_trait_impl_blocks: Vec<RustImplBlock> = Vec::new();

        // Only need to look in the same or child scopes of the item definition since an impl on an item cannot be in a parent scope
        let possible_scopes = self.scopes.iter().rev().take_while(|s| {
            s.item_definitons
                .iter()
                // .any(|item_def| item_def.ident == item_path_seg.ident)
                .any(|item_def| &item_def.ident == item_name)
        });
        let mut all_impl_blocks = possible_scopes
            .map(|s| s.impl_blocks.iter().cloned())
            .flatten()
            .collect::<Vec<_>>();
        all_impl_blocks.extend(self.impl_blocks.iter().cloned());
        // dbg!(&all_impl_blocks);

        let mut prev_found_traits_length = found_traits.len();

        // Each time we find new traits we need to again look for matching traits, and repeat this until we don't find any new traits
        let mut start = true;
        // while found_traits_previous.len() < found_traits.len() || start {
        // TODO this works fine for `impl Foo for Bar`, need to fix for `impl Foo for T`
        while start {
            start = false;
            found_traits_previous = found_traits.clone();
            found_traits.clear();

            for impl_block in &all_impl_blocks {
                // TODO this needs extending to handle matching any target type, rather than just user structs
                match &impl_block.target {
                    RustType::TypeParam(rust_type_param) => {
                        todo!();
                        // If the target is a type param then we must be implementing a trait
                        let rust_trait = impl_block.trait_.clone().unwrap();

                        // Get bounds on type param
                        let type_param_bounds = &impl_block
                            .generics
                            .iter()
                            .find(|generic| generic.ident == rust_type_param.name)
                            .unwrap()
                            .trait_bounds;

                        // Does our struct impl all of these traits?
                        let struct_impls_all_bounds = type_param_bounds
                            .iter()
                            .all(|type_param_bound| found_traits.contains(type_param_bound));
                        if struct_impls_all_bounds {
                            found_traits.push(rust_trait);
                            matched_trait_impl_blocks.push(impl_block.clone());
                        }
                    }
                    RustType::StructOrEnum(struct_type_params, struct_module_path, struct_name) => {
                        if let Some(impl_trait) = &impl_block.trait_ {
                            let types_match = struct_or_enum_types_match(
                                &impl_block.target,
                                &item_generics,
                                &item_module_path,
                                &item_def.ident,
                            );

                            if types_match {
                                found_traits.push(impl_trait.clone());
                                matched_trait_impl_blocks.push(impl_block.clone());
                            }
                            // TODO Trying to get the concrete params at this point doesn't make senese because quite often it is the argument(s) to the associated fn which will determine the concrete params
                        }
                    }
                    RustType::MutRef(_) => todo!(),
                    RustType::Ref(_) => todo!(),
                    _ => todo!(),
                }
            }
        }

        // Now we have all the impl blocks, we can look for the method in said impl blocks
        // We also need to check the traits themselves incase the method is a default implementation
        let scoped_traits = self
            .scopes
            .iter()
            .rev()
            .map(|s| {
                s.trait_definitons
                    .iter()
                    .filter(|trait_def| found_traits.contains(&(None, trait_def.name.clone())))
            })
            .flatten();
        let module_level_traits = self
            .modules
            .iter()
            .map(|module| {
                module.trait_definitons.iter().filter(|trait_def| {
                    found_traits.contains(&(Some(module.path.clone()), trait_def.name.clone()))
                })
            })
            .flatten();
        // TODO add default impl items to traits
        // let default_trait_method = scoped_traits
        //     .chain(module_level_traits)
        //     .find_map(|trait_def| {
        //         trait_def
        //             .items
        //             .iter()
        //             .find(|impl_item| impl_item.ident == sub_path.ident)
        //             .cloned()
        //     });

        // It is not possible to have impls with the same method name, so we should at most match 1 impl item/method
        // dbg!(&matched_trait_impl_blocks);
        assert!(
            matched_trait_impl_blocks
                .iter()
                .filter_map(|impl_block| {
                    impl_block
                        .items
                        .iter()
                        .find(|impl_item| impl_item.ident == sub_path.ident)
                })
                .count()
                <= 1
        );
        let module_level_impl_method = matched_trait_impl_blocks
            .iter()
            .find_map(|impl_block| {
                impl_block
                    .items
                    .iter()
                    .find(|impl_item| impl_item.ident == sub_path.ident)
            })
            .cloned();

        // Use xor because we should not have both a scoped and module level impl method, only either or
        let impl_method = scoped_impl_method.xor(module_level_impl_method);
        impl_method
    }
}

enum VarItemFn {
    Var(ScopedVar),
    StructOrEnum(ItemDefinition),
    Fn(FnInfo),
}

// TODO I think we want to add *usertype* impls and *impl trait for usertype* (in both cases where the usertype is not generic) as methods to the type's class, but for all other cases, eg *impl trait for T* impls we want to keep the trait impl as an object of fns/class with methods and then still add equivalent methods to the type classes but rather than include the fn body, just point to these fns/methods in the impl block object. This avoids eg potentially duplicating fn bodys on *all* type classes if eg we have `impl<T> MyTrait for T` {}. Though the cost of duplicating the fn bodys should be irrelevant with compression... so the best reason to do this is probably to avoid noise when reading the transpiled JS, and to make the code feel more idiomatic/familiar to the Rust dev.
// What about somthing like `impl MyTrait for Foo<Bar> { ... }` and `impl MyTrait for Foo<Baz> { ... }`. The methods should live on the Foo class, but the bodies/impls are different depending on the generic eg `let foo = Foo(Bar()).some_trait_method();` vs `let foo = Foo(Baz()).some_trait_method();`
// eg `let foo = Foo(Bar()).some_trait_method();` vs `let foo = Foo(Baz()).some_trait_method();`
// Or even just `impl Foo<Bar> { ... }` and `impl Foo<Baz> { ... }`.
// So need to monomorphize generic types that

/// Match impl items to the classes in a `JsStmtModule`'s stmts and update the classes, recursively doing the same thing for any sub modules
fn update_classes(
    js_stmt_modules: &mut Vec<JsModule>,
    // impl_items: &Vec<ImplItemTemp>,
    impl_items: &Vec<RustImplBlock>,
    default_trait_impls_class_mapping: &Vec<(String, String)>,
    default_trait_impls: &Vec<(String, JsImplItem)>,
) {
    let span = debug_span!("update_classes");
    let _guard = span.enter();
    // dbg!(&js_stmt_modules);
    // dbg!(&impl_items);
    for js_stmt_module in js_stmt_modules {
        let span = debug_span!("update_classes for js_stmt_module", path = ?js_stmt_module.module_path, name = ?js_stmt_module.name);
        let _guard = span.enter();

        update_classes_js_stmts(
            &mut js_stmt_module.stmts,
            impl_items,
            // default_trait_impls_class_mapping,
            // default_trait_impls,
        )
    }
}

// For impl blocks targetting a concrete type (including when implementing a trait), we want to add the items directly to the JS class, because no other types/classes are using the same implementation. For blocks targetting a generic type eg `Foo<T>` or `T`, we need to check if multiple classes are using the same method impls and if so keep the impl standalone and point to the impl from a class method.
fn update_classes_js_stmts(
    js_stmts: &mut Vec<JsStmt>,
    impl_items: &Vec<RustImplBlock>,
    // default_trait_impls_class_mapping: &Vec<(String, String)>,
    // default_trait_impls: &Vec<(String, JsImplItem)>,
) {
    for stmt in js_stmts {
        match stmt {
            JsStmt::Class(js_class) => {
                let span = debug_span!("update_classes for js_class", name = ?js_class.name);
                let _guard = span.enter();
                // dbg!(&js_class);
                // dbg!(&impl_items);
                // add impl methods to class
                for impl_block in impl_items.clone() {
                    // TODO impl could be in another module?
                    let impl_target_name = match impl_block.target {
                        RustType::StructOrEnum(_, _, name) => name,
                        _ => todo!(),
                    };
                    if js_class.name == impl_target_name {
                        for impl_item in impl_block.items {
                            match impl_item.item {
                                RustImplItemItem::Fn(private, static_, fn_info, js_fn) => {
                                    let FnInfo {
                                        ident,
                                        inputs_types,
                                        generics,
                                        return_type,
                                    } = fn_info;

                                    // If the struct is generic *and* already has a method with the same name as one in the impl block, then we need to monomorphize the struct
                                    if js_class.methods.iter().any(|method| method.0 == ident) {
                                        todo!("need to monomorphize");
                                    } else {
                                        js_class.methods.push((ident, private, static_, js_fn));
                                    }
                                }
                                RustImplItemItem::Const(js_local) => {
                                    js_class.static_fields.push(js_local);
                                }
                            }
                        }
                        // match impl_item.item_stmt {
                        //     JsImplItem::ClassStatic(js_local) => {
                        //         js_class.static_fields.push(js_local);
                        //     }
                        //     JsImplItem::ClassMethod(name, private, static_, js_fn) => {
                        //         js_class.methods.push((name, private, static_, js_fn));
                        //     }
                        //     stmt => {
                        //         dbg!(stmt);
                        //         panic!("this JsStmt cannot be an impl item")
                        //     }
                        // }
                    }
                }

                // TODO when adding a default impl to a class, we need to know which module/scope it came from in case there are two trait with the same name
                // TODO also need to only add if there is not an impl which overrides the default
                // add trait methods with default impl to class
                // let trait_names = default_trait_impls_class_mapping.iter().filter_map(
                //     |(class_name, trait_name)| (class_name == &js_class.name).then_some(trait_name),
                // );
                // for trait_name in trait_names {
                //     let impl_items =
                //         default_trait_impls
                //             .iter()
                //             .filter_map(|(trait_name2, js_impl_item)| {
                //                 (trait_name == trait_name2).then_some(js_impl_item.clone())
                //             });
                //     for js_impl_item in impl_items {
                //         match js_impl_item {
                //             JsImplItem::ClassStatic(js_local) => {
                //                 js_class.static_fields.push(js_local);
                //             }
                //             JsImplItem::ClassMethod(name, private, static_, js_fn) => {
                //                 js_class.methods.push((name, private, static_, js_fn));
                //             }
                //             stmt => {
                //                 dbg!(stmt);
                //                 panic!("this JsStmt cannot be an impl item")
                //             }
                //         }
                //     }
                // }
            }
            // JsStmt::Module(js_stmt_module) => update_classes(js_stmt_module, impl_items.clone()),
            _ => {}
        }
    }
}

// Similarly to `update_classes`, we need to do a pass to replace all use of top level items like `myFunc()`, `new SomeClass()`, `SomeClass.associatedFunc()` with `this.myFunc()`, `new this.SomeClass()`, `this.SomeClass.associatedFunc()`. This means first getting the names of the top level items, and then not just iterating through the module's statements but though every expression at a top level item could be called absolutely anywhere, eg in an `if` condition.
// What about where we are inside a class method so this refers to the class, not the module?
// Solutions:
// add a unique name like `this.moduleThis = this;` - not possible because it still exists on the `this` which class methods don't have access to.
// add `this.moduleThis` to class objects in init function??
// replace `myFunc()` with `this.thisModule.myFunc();` and add below to init script:
// this.colorModule.greenModule.greenClass.prototype.thisModule = this.colorModule.greenModule;`

// Having "crate" in the module path is useful for representing that the top level module is indeed a module, and for giving it a name that can be looked up in the list. However, it is annoying for eg using the path to create a filepath from

// TODO `names` is probably redundant as we have the same data in `modules`
// Update names
// Determine which names are not globally unique and need namespacing
// TODO should cache the syn::File parsed for each module to avoid repeating this expensive operation (read file and parse) during the parse stage
/// gets names of module level items, creates `ModuleData` for each module, and adds `use` data to module's `.use_mapping`
fn extract_data(
    module_level_items: bool,
    items: &Vec<Item>,
    // Same as `global_data.crate_path`, used for prepending module filepaths, except we don't have a `GlobalData` yet so we pass it in directly
    // None if we are extracting data for a single file or snippet, rather than an actual crate (so no `mod foo` allowed)
    // TODO crate_path might use hiphens instead of underscore as a word seperator, so need to ensure it is only used for file paths, and not module paths
    crate_path: &Option<PathBuf>,
    module_path: &mut Vec<String>,
    // (module path, name)
    names: &mut Vec<(Vec<String>, String)>,
    scoped_names: &mut Vec<(Vec<String>, String)>,
    modules: &mut Vec<ModuleData>,
) {
    // let mut module_path_with_crate = vec!["crate".to_string()];
    // module_path_with_crate.extend(module_path.clone());
    // let current_module_data = modules
    //     .iter_mut()
    //     .find(|module| module.path == *module_path)
    //     .unwrap();
    // let defined_names = &mut current_module_data.defined_names;

    // dbg!(&module_path);

    // TODO the code for eg module.item_definitions.push(...) is a duplicated also for scope.item_definitons.push(...). Remove this duplication.
    for item in items {
        match item {
            Item::Const(item_const) => {
                let const_name = item_const.ident.to_string();
                if module_level_items {
                    names.push((module_path.clone(), const_name.clone()));
                    let current_module_data = modules
                        .iter_mut()
                        .find(|module| module.path == *module_path)
                        .unwrap();
                    match item_const.vis {
                        Visibility::Public(_) => current_module_data
                            .pub_definitions
                            .push(item_const.ident.to_string()),
                        Visibility::Restricted(_) => todo!(),
                        Visibility::Inherited => current_module_data
                            .private_definitions
                            .push(item_const.ident.to_string()),
                    }
                } else {
                    scoped_names.push((module_path.clone(), const_name.clone()));
                }
            }
            Item::Enum(item_enum) => {
                let enum_name = item_enum.ident.to_string();
                if module_level_items {
                    names.push((module_path.clone(), enum_name.clone()));

                    let current_module_data = modules
                        .iter_mut()
                        .find(|module| module.path == *module_path)
                        .unwrap();
                    match item_enum.vis {
                        Visibility::Public(_) => current_module_data
                            .pub_definitions
                            .push(item_enum.ident.to_string()),
                        Visibility::Restricted(_) => todo!(),
                        Visibility::Inherited => current_module_data
                            .private_definitions
                            .push(item_enum.ident.to_string()),
                    }
                } else {
                    scoped_names.push((module_path.clone(), enum_name.clone()));
                }
            }
            Item::ExternCrate(_) => todo!(),
            Item::Fn(item_fn) => {
                let fn_name = item_fn.sig.ident.to_string();
                if module_level_items {
                    names.push((module_path.clone(), fn_name.clone()));

                    let current_module_data = modules
                        .iter_mut()
                        .find(|module| module.path == *module_path)
                        .unwrap();
                    match item_fn.vis {
                        Visibility::Public(_) => current_module_data.pub_definitions.push(fn_name),
                        Visibility::Restricted(_) => todo!(),
                        Visibility::Inherited => {
                            current_module_data.private_definitions.push(fn_name)
                        }
                    }
                } else {
                    scoped_names.push((module_path.clone(), fn_name));
                }

                // Record scoped ident names so we can ensure any module level items with the same name are namespaced
                let items = item_fn
                    .clone()
                    .block
                    .stmts
                    .into_iter()
                    .filter_map(|s| match s {
                        Stmt::Item(item) => Some(item),
                        _ => None,
                    })
                    .collect::<Vec<_>>();

                extract_data(
                    false,
                    &items,
                    crate_path,
                    module_path,
                    names,
                    scoped_names,
                    modules,
                )
            }
            Item::ForeignMod(_) => todo!(),
            Item::Impl(item_impl) => {
                // Record scoped ident names so we can ensure any module level items with the same name are namespaced
                let items = item_impl
                    .clone()
                    .items
                    .into_iter()
                    .filter_map(|impl_item| match impl_item {
                        ImplItem::Fn(impl_item_fn) => {
                            let items =
                                impl_item_fn.block.stmts.into_iter().filter_map(
                                    |stmt| match stmt {
                                        Stmt::Item(item) => Some(item),
                                        _ => None,
                                    },
                                );
                            Some(items)
                        }
                        _ => None,
                    })
                    .flatten()
                    .collect::<Vec<_>>();

                extract_data(
                    false,
                    &items,
                    crate_path,
                    module_path,
                    names,
                    scoped_names,
                    modules,
                )
            }
            Item::Macro(_) => {}
            Item::Mod(item_mod) => {
                let current_module_data = modules
                    .iter_mut()
                    .find(|module| module.path == *module_path)
                    .unwrap();
                match item_mod.vis {
                    Visibility::Public(_) => current_module_data
                        .pub_submodules
                        .push(item_mod.ident.to_string()),
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => current_module_data
                        .private_submodules
                        .push(item_mod.ident.to_string()),
                }

                let parent_name = module_path.last().map(|x| x.clone());
                module_path.push(item_mod.ident.to_string());

                let mut partial_module_data = ModuleData {
                    name: item_mod.ident.to_string(),
                    parent_name,
                    path: module_path.clone(),
                    pub_definitions: Vec::new(),
                    private_definitions: Vec::new(),
                    pub_submodules: Vec::new(),
                    private_submodules: Vec::new(),
                    pub_use_mappings: Vec::new(),
                    private_use_mappings: Vec::new(),
                    resolved_mappings: Vec::new(),
                    fn_info: Vec::new(),
                    item_definitons: Vec::new(),
                    trait_definitons: Vec::new(),
                    consts: Vec::new(),
                    items: Vec::new(),
                };

                // NOTE we do the `modules.push(ModuleData { ...` below because we need to get the module items from the different content/no content branches
                if let Some(content) = &item_mod.content {
                    partial_module_data.items = content.1.clone();
                    modules.push(partial_module_data);

                    // TODO how does `mod bar { mod foo; }` work?
                    extract_data(
                        true,
                        &content.1,
                        crate_path,
                        module_path,
                        names,
                        scoped_names,
                        modules,
                    );
                } else {
                    if let Some(crate_path2) = crate_path {
                        let mut file_path = crate_path2.clone();
                        file_path.push("src");
                        // IMPORTANT TODO need to check for "crate" *and* "my_external_crate", and also use the corrent `crate_path`
                        if *module_path == ["crate"] {
                            file_path.push("main.rs");
                        } else {
                            let mut module_path_copy = module_path.clone();
                            // remove "crate"
                            module_path_copy.remove(0);
                            let last = module_path_copy.last_mut().unwrap();
                            last.push_str(".rs");
                            file_path.extend(module_path_copy);
                        }

                        let code = fs::read_to_string(&file_path).unwrap();
                        let file = syn::parse_file(&code).unwrap();

                        partial_module_data.items = file.items.clone();
                        modules.push(partial_module_data);
                        extract_data(
                            true,
                            &file.items,
                            crate_path,
                            module_path,
                            names,
                            scoped_names,
                            modules,
                        );
                    } else {
                        panic!("`mod foo` is not allowed in files/modules/snippets, only crates")
                    }
                }
                module_path.pop();
            }
            Item::Static(_) => todo!(),
            Item::Struct(item_struct) => {
                let struct_name = item_struct.ident.to_string();
                names.push((module_path.clone(), struct_name.clone()));

                let current_module_data = modules
                    .iter_mut()
                    .find(|module| module.path == *module_path)
                    .unwrap();
                match item_struct.vis {
                    Visibility::Public(_) => current_module_data
                        .pub_definitions
                        .push(item_struct.ident.to_string()),
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => current_module_data
                        .private_definitions
                        .push(item_struct.ident.to_string()),
                }
            }
            Item::Trait(item_trait) => {
                names.push((module_path.clone(), item_trait.ident.to_string()));

                // TODO adding traits to the definitions like below means their names will be taken into account when finding duplicates and namespacing, which we don't want because traits don't actually appear in the transpiled JS
                let current_module_data = modules
                    .iter_mut()
                    .find(|module| module.path == *module_path)
                    .unwrap();
                match item_trait.vis {
                    Visibility::Public(_) => current_module_data
                        .pub_definitions
                        .push(item_trait.ident.to_string()),
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => current_module_data
                        .private_definitions
                        .push(item_trait.ident.to_string()),
                }
            }
            Item::TraitAlias(_) => todo!(),
            Item::Type(_) => todo!(),
            Item::Union(_) => todo!(),
            Item::Use(item_use) => {
                let module = modules
                    .iter_mut()
                    .find(|module| &module.path == module_path)
                    .unwrap();
                handle_item_use(&item_use, ItemUseModuleOrScope::Module(module));
            }
            Item::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }
}

fn extract_data_populate_item_definitions(
    // items: &Vec<Item>,
    // None if we are extracted data for a single file or snippet, rather than an actual crate (so no `mod foo` allowed)
    // crate_path: Option<&PathBuf>,
    // module_path: &mut Vec<String>,
    // (module path, name)
    // names: &mut Vec<(Vec<String>, String)>,
    // modules: &mut Vec<ModuleData>,
    global_data: &mut GlobalData,
) {
    // TODO the code for eg module.item_definitions.push(...) is a duplicated also for scope.item_definitons.push(...). Remove this duplication.

    // This is because parse_types_for_populate_item_definitions needs a access to .pub_definitions etc in global_data from `extract_data()` but we are taking an immutable ref first
    let global_data_copy = global_data.clone();

    for module in &mut global_data.modules {
        debug_span!(
            "extract_data_populate_item_definitions module: {:?}",
            module_path = ?module.path
        );
        for item in &module.items {
            match item {
                Item::Const(item_const) => {
                    let const_name = item_const.ident.to_string();
                    let rust_type = parse_types_for_populate_item_definitions(
                        &item_const.ty,
                        &Vec::new(),
                        &module.path,
                        &global_data_copy,
                    );
                    module.consts.push(ConstDef {
                        name: const_name,
                        type_: rust_type,
                        syn_object: item_const.clone(),
                    });
                }
                Item::Enum(item_enum) => {
                    let enum_name = item_enum.ident.to_string();

                    // Make ItemDefinition
                    let generics = item_enum
                        .generics
                        .params
                        .iter()
                        .map(|p| match p {
                            GenericParam::Lifetime(_) => todo!(),
                            GenericParam::Type(type_param) => type_param.ident.to_string(),
                            GenericParam::Const(_) => todo!(),
                        })
                        .collect::<Vec<_>>();
                    let members_for_scope = item_enum
                        .variants
                        .iter()
                        .map(|v| EnumVariantInfo {
                            ident: v.ident.to_string(),
                            inputs: v
                                .fields
                                .iter()
                                .map(|f| {
                                    let input_type = parse_types_for_populate_item_definitions(
                                        &f.ty,
                                        &generics,
                                        &module.path,
                                        &global_data_copy,
                                    );
                                    match &f.ident {
                                        Some(input_name) => EnumVariantInputsInfo::Named {
                                            ident: input_name.to_string(),
                                            input_type,
                                        },
                                        None => EnumVariantInputsInfo::Unnamed(input_type),
                                    }
                                })
                                .collect::<Vec<_>>(),
                        })
                        .collect::<Vec<_>>();

                    module.item_definitons.push(ItemDefinition {
                        ident: enum_name,
                        is_copy: item_enum.attrs.iter().any(|attr| match &attr.meta {
                            Meta::Path(_) => todo!(),
                            Meta::List(meta_list) => {
                                let segs = &meta_list.path.segments;
                                if segs.len() == 1 && segs.first().unwrap().ident == "derive" {
                                    let tokens = format!("({})", meta_list.tokens);
                                    let trait_tuple =
                                        syn::parse_str::<syn::TypeTuple>(&tokens).unwrap();
                                    trait_tuple.elems.iter().any(|elem| match elem {
                                        Type::Path(type_path) => {
                                            let segs = &type_path.path.segments;
                                            // TODO `Copy` could have been shadowed to need to do a proper lookup for trait with name `Copy` to check whether it is std::Copy or not.
                                            segs.len() == 1 && segs.first().unwrap().ident == "Copy"
                                        }
                                        _ => todo!(),
                                    })
                                } else {
                                    false
                                }
                            }
                            Meta::NameValue(_) => todo!(),
                        }),
                        generics,
                        struct_or_enum_info: StructOrEnumDefitionInfo::Enum(EnumDefinitionInfo {
                            members: members_for_scope,
                            syn_object: item_enum.clone(),
                        }),
                    });
                }
                Item::ExternCrate(_) => todo!(),
                Item::Fn(item_fn) => {
                    let generics = item_fn
                        .sig
                        .generics
                        .params
                        .iter()
                        .filter_map(|g| match g {
                            GenericParam::Lifetime(_) => None,
                            GenericParam::Type(type_param) => Some(type_param.ident.to_string()),
                            GenericParam::Const(_) => todo!(),
                        })
                        .collect::<Vec<_>>();

                    let inputs_types = item_fn
                        .sig
                        .inputs
                        .iter()
                        .filter_map(|input| match input {
                            FnArg::Receiver(_) => None,
                            FnArg::Typed(pat_type) => {
                                Some(parse_types_for_populate_item_definitions(
                                    &*pat_type.ty,
                                    &generics,
                                    &module.path,
                                    &global_data_copy,
                                ))
                            }
                        })
                        .collect::<Vec<_>>();

                    let return_type = match &item_fn.sig.output {
                        ReturnType::Default => RustType::Unit,
                        ReturnType::Type(_, type_) => parse_types_for_populate_item_definitions(
                            &*type_,
                            &generics,
                            &module.path,
                            &global_data_copy,
                        ),
                    };
                    module.fn_info.push(FnInfo {
                        ident: item_fn.sig.ident.to_string(),
                        inputs_types,
                        generics,
                        return_type,
                    })
                }
                Item::ForeignMod(_) => todo!(),
                Item::Impl(_) => {
                    // TODO IMPORTANT currently we are adding top level impl blocks to `global_data.impl_blocks` in handle_item_impl(). It would be better to push (non-scoped) impl blocks here, so that they are already available if a method defined on the impl is called before the impl block itself is reached/parsed by `handle_item_impl()`. However we still need to find a way to solve this problem for the scoped impl blocks anyway. Leave it as is for now until we do some refactoring and deduplication, to avoid need to repeat a bunch of code here.
                }
                Item::Macro(_) => {}
                Item::Mod(item_mod) => {}
                Item::Static(_) => todo!(),
                Item::Struct(item_struct) => {
                    let struct_name = item_struct.ident.to_string();

                    // Make ItemDefinition
                    let generics = item_struct
                        .generics
                        .params
                        .iter()
                        .map(|p| match p {
                            GenericParam::Lifetime(_) => todo!(),
                            GenericParam::Type(type_param) => type_param.ident.to_string(),
                            GenericParam::Const(_) => todo!(),
                        })
                        .collect::<Vec<_>>();

                    let generics_type_params = generics
                        .iter()
                        .map(|name| RustTypeParam {
                            name: name.clone(),
                            type_: RustTypeParamValue::Unresolved,
                        })
                        .collect::<Vec<_>>();

                    let fields = if item_struct.fields.len() == 0 {
                        StructFieldInfo::UnitStruct
                    } else if item_struct.fields.iter().next().unwrap().ident.is_some() {
                        StructFieldInfo::RegularStruct(
                            item_struct
                                .fields
                                .iter()
                                .map(|f| {
                                    (
                                        f.ident.as_ref().unwrap().to_string(),
                                        parse_types_for_populate_item_definitions(
                                            &f.ty,
                                            &generics,
                                            &module.path,
                                            &global_data_copy,
                                        ),
                                    )
                                })
                                .collect::<Vec<_>>(),
                        )
                    } else {
                        StructFieldInfo::TupleStruct(
                            item_struct
                                .fields
                                .iter()
                                .map(|f| {
                                    parse_types_for_populate_item_definitions(
                                        &f.ty,
                                        &generics,
                                        &module.path,
                                        &global_data_copy,
                                    )
                                })
                                .collect::<Vec<_>>(),
                        )
                    };

                    module.item_definitons.push(ItemDefinition {
                        ident: item_struct.ident.to_string(),
                        is_copy: item_struct.attrs.iter().any(|attr| match &attr.meta {
                            Meta::Path(_) => todo!(),
                            Meta::List(meta_list) => {
                                let segs = &meta_list.path.segments;
                                if segs.len() == 1 && segs.first().unwrap().ident == "derive" {
                                    let tokens = format!("({})", meta_list.tokens);
                                    let trait_tuple =
                                        syn::parse_str::<syn::TypeTuple>(&tokens).unwrap();
                                    trait_tuple.elems.iter().any(|elem| match elem {
                                        Type::Path(type_path) => {
                                            let segs = &type_path.path.segments;
                                            // TODO `Copy` could have been shadowed to need to do a proper lookup for trait with name `Copy` to check whether it is std::Copy or not.
                                            segs.len() == 1 && segs.first().unwrap().ident == "Copy"
                                        }
                                        _ => todo!(),
                                    })
                                } else {
                                    false
                                }
                            }
                            Meta::NameValue(_) => todo!(),
                        }),
                        generics,
                        struct_or_enum_info: StructOrEnumDefitionInfo::Struct(
                            StructDefinitionInfo {
                                fields,
                                syn_object: Some(item_struct.clone()),
                            },
                        ),
                    });
                }
                Item::Trait(item_trait) => module.trait_definitons.push(RustTraitDefinition {
                    name: item_trait.ident.to_string(),
                }),
                Item::TraitAlias(_) => todo!(),
                Item::Type(_) => todo!(),
                Item::Union(_) => todo!(),
                Item::Use(_) => {}
                Item::Verbatim(_) => todo!(),
                _ => todo!(),
            }
        }
    }
}

fn update_dup_names(duplicates: &mut Vec<Duplicate>) {
    // We take a copy to make sure both duplicates have a path segment added to their namespace, rather than just the first one that is found
    let dups_copy = duplicates.clone();
    for dup in duplicates.iter_mut() {
        if dups_copy
            .iter()
            .filter(|dup_copy| dup.name == dup_copy.name && dup.namespace == dup_copy.namespace)
            .count()
            > 1
        {
            if dup.module_path != vec!["crate"] {
                dup.namespace.insert(0, dup.module_path.pop().unwrap())
            }
        }
    }
}

fn push_rust_types(global_data: &GlobalData, mut js_stmts: Vec<JsStmt>) -> Vec<JsStmt> {
    // We want to insert prelude stmts at the beginning of js_stmts, but if we do that per item we will reverse the order they appear in the source files. Instead we push them to `prelude_stmts` and then insert that in one go
    let mut prelude_stmts = Vec::new();

    let rust_prelude_types = &global_data.rust_prelude_types;

    if rust_prelude_types.rust_integer {
        let js_class = JsStmt::Class(JsClass {
            public: false,
            export: false,
            tuple_struct: false,
            name: "RustInteger".to_string(),
            inputs: vec!["inner".to_string()],
            static_fields: Vec::new(),
            methods: Vec::new(),
        });
        prelude_stmts.push(js_class);
    }
    if rust_prelude_types.rust_string {
        let js_class = JsStmt::Class(JsClass {
            public: false,
            export: false,
            tuple_struct: false,
            name: "RustString".to_string(),
            inputs: vec!["inner".to_string()],
            static_fields: Vec::new(),
            methods: Vec::new(),
        });
        prelude_stmts.push(js_class);
    }
    if rust_prelude_types.rust_array_copy {
        let js_stmt = JsStmt::Raw(
            r"
                Array.prototype.copy = function () {
                    return JSON.parse(JSON.stringify(this));
                };
            "
            .to_string(),
        );
        prelude_stmts.push(js_stmt);
    }

    if rust_prelude_types.vec {
        let mut methods = Vec::new();
        methods.push((
            "new".to_string(),
            false,
            true,
            JsFn {
                iife: false,
                export: false,
                public: false,
                async_: false,
                is_method: true,
                name: "new".to_string(),
                input_names: Vec::new(),
                body_stmts: vec![JsStmt::Raw("this.vec = [];".to_string())],
            },
        ));
        methods.push((
            "push".to_string(),
            false,
            false,
            JsFn {
                iife: false,
                export: false,
                public: false,
                async_: false,
                is_method: true,
                name: "push".to_string(),
                input_names: vec!["elem".to_string()],
                body_stmts: vec![JsStmt::Raw("this.vec.push(elem);".to_string())],
            },
        ));
        prelude_stmts.push(JsStmt::Class(JsClass {
            export: false,
            public: false,
            name: "Vec".to_string(),
            tuple_struct: false,
            inputs: Vec::new(),
            static_fields: Vec::new(),
            methods,
        }));
    }

    if rust_prelude_types.option {
        let code = include_str!("rust_prelude/option.rs");
        let modules = from_file(code, false);
        assert_eq!(modules.len(), 1);
        let option_module = &modules[0];

        for stmt in &option_module.stmts {
            match stmt {
                JsStmt::Class(js_class) => {
                    if js_class.name == "Option" {
                        prelude_stmts.push(stmt.clone());
                    }
                }
                JsStmt::ClassMethod(_, _, _, _) => todo!(),
                JsStmt::ClassStatic(_) => todo!(),
                // JsStmt::Local(js_local) => match &js_local.lhs {
                //     LocalName::Single(name) => {
                //         if name == "Some" || name == "None" {
                //             js_stmts.insert(0, stmt.clone());
                //         }
                //     }
                //     LocalName::DestructureObject(_) => todo!(),
                //     LocalName::DestructureArray(_) => todo!(),
                // },
                JsStmt::Local(_) => todo!(),
                JsStmt::Expr(_, _) => todo!(),
                JsStmt::Import(_, _, _) => todo!(),
                JsStmt::Function(_) => todo!(),
                JsStmt::ScopeBlock(_) => todo!(),
                // JsStmt::TryBlock(_) => todo!(),
                // JsStmt::CatchBlock(_, _) => todo!(),
                JsStmt::Raw(_) => todo!(),
                JsStmt::Comment(_) => todo!(),
            }
        }
    }
    if rust_prelude_types.some {
        prelude_stmts.push(JsStmt::Raw("var Some = Option.Some;".to_string()))
    }
    if rust_prelude_types.none {
        prelude_stmts.push(JsStmt::Raw("var None = Option.None;".to_string()))
    }

    if rust_prelude_types.string_prototype_extensions {
        prelude_stmts.push(JsStmt::Raw(
            include_str!("../../ravascript/tests/string_prototype_extensions.js").to_string(),
        ))
    }
    if rust_prelude_types.number_prototype_extensions {
        prelude_stmts.push(JsStmt::Raw(
            include_str!("../../ravascript/tests/number_prototype_extensions.js").to_string(),
        ))
    }

    if rust_prelude_types.integer || rust_prelude_types.float {
        let code = include_str!("rust_prelude/number.rs");
        let modules = from_file(code, false);
        assert_eq!(modules.len(), 1);
        let number_module = &modules[0];

        for stmt in &number_module.stmts {
            match stmt {
                JsStmt::Class(js_class) => {
                    if js_class.name == "RustInteger" {
                        if rust_prelude_types.integer {
                            prelude_stmts.push(stmt.clone());
                        }
                    } else if js_class.name == "RustFloat" {
                        if rust_prelude_types.float {
                            prelude_stmts.push(stmt.clone());
                        }
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        }
    }

    if rust_prelude_types.string {
        let code = include_str!("rust_prelude/string.rs");
        let modules = from_file(code, false);
        assert_eq!(modules.len(), 1);
        let string_module = &modules[0];

        for stmt in &string_module.stmts {
            match stmt {
                JsStmt::Class(js_class) => {
                    // TODO don't need this if check?
                    if js_class.name == "RustString" {
                        prelude_stmts.push(stmt.clone());
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        }
    }

    if rust_prelude_types.bool {
        let code = include_str!("rust_prelude/bool.rs");
        let modules = from_file(code, false);
        assert_eq!(modules.len(), 1);
        let bool_module = &modules[0];

        for stmt in &bool_module.stmts {
            match stmt {
                JsStmt::Class(js_class) => {
                    if js_class.name == "RustBool" {
                        prelude_stmts.push(stmt.clone());
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        }
    }

    if rust_prelude_types.equals {
        // TODO write the function in Rust and transpile it
        prelude_stmts.push(JsStmt::Raw(
            "function eq(lhs, rhs) { return JSON.stringify(lhs) === JSON.stringify(rhs); }"
                .to_string(),
        ))
    }
    prelude_stmts.append(&mut js_stmts);
    prelude_stmts
}

pub fn process_items(
    mut items: Vec<Item>,
    crate_path: Option<PathBuf>,
    // TODO I don't think there is much point in supporting generation without "Rust types" so remove this flag
    with_rust_types: bool,
) -> Vec<JsModule> {
    let mut modules = Vec::new();
    modules.push(ModuleData {
        name: "crate".to_string(),
        parent_name: None,
        path: vec!["crate".to_string()],
        pub_definitions: Vec::new(),
        private_definitions: Vec::new(),
        pub_submodules: Vec::new(),
        private_submodules: Vec::new(),
        pub_use_mappings: Vec::new(),
        private_use_mappings: Vec::new(),
        resolved_mappings: Vec::new(),
        fn_info: Vec::new(),
        item_definitons: Vec::new(),
        trait_definitons: Vec::new(),
        consts: Vec::new(),
        items: items.clone(),
    });
    let get_names_module_path = vec!["crate".to_string()];

    // NOTE would need to take into account scoped item names if we hoisted scoped items to module level
    let mut names_for_finding_duplicates = Vec::new();
    let mut scoped_names_for_finding_duplicates = Vec::new();
    // gets names of module level items, creates `ModuleData` for each module, and adds `use` data to module's `.use_mapping`
    // dbg!(&modules);
    extract_data(
        true,
        &items,
        &crate_path,
        &mut get_names_module_path.clone(),
        &mut names_for_finding_duplicates,
        &mut scoped_names_for_finding_duplicates,
        &mut modules,
    );
    // dbg!(&modules);

    // TODO web prelude should probably be a cargo-js/ravascript module, not an entire crate? If people are going to have to add ravascript as a dependency as well as install the CLI then yes, otherwise if they have no dependency to add other than the web prelude, may as well make it a specific crate?
    // Now that with have extracted data for the main.rs/lib.rs, we do the same for third party crates.
    // Currently this is only the web prelude for which we need to `extract_data` for deduplicating/namespacing and getting use mappings for resolving paths, and `extract_data_populate_item_definitions` for getting item definitions to eg lookup methods etc, but we do not actually need to parse to a JS AST.
    let web_prelude_crate_path = "../web-prelude";
    let web_prelude_entry_point_path = PathBuf::new()
        .join(web_prelude_crate_path)
        .join("src")
        .join("lib.rs");
    let code = fs::read_to_string(web_prelude_entry_point_path).unwrap();
    let file = syn::parse_file(&code).unwrap();
    let prelude_items = file.items;
    modules.push(ModuleData {
        name: "web_prelude".to_string(),
        parent_name: None,
        path: vec!["web_prelude".to_string()],
        pub_definitions: Vec::new(),
        private_definitions: Vec::new(),
        pub_submodules: Vec::new(),
        private_submodules: Vec::new(),
        pub_use_mappings: Vec::new(),
        private_use_mappings: Vec::new(),
        resolved_mappings: Vec::new(),
        fn_info: Vec::new(),
        item_definitons: Vec::new(),
        trait_definitons: Vec::new(),
        consts: Vec::new(),
        items: prelude_items.clone(),
    });

    extract_data(
        true,
        &prelude_items,
        &Some(web_prelude_crate_path.into()),
        // &mut get_names_module_path.clone(),
        &mut vec!["web_prelude".to_string()],
        &mut names_for_finding_duplicates,
        &mut scoped_names_for_finding_duplicates,
        &mut modules,
    );
    // dbg!(&modules);

    // find duplicates
    // TODO account for local functions which shadow these names
    // (name space, module path (which gets popped), name, original module path)

    let mut duplicates = Vec::new();
    // TODO surely we want
    for name in &names_for_finding_duplicates {
        if names_for_finding_duplicates
            .iter()
            // NOTE given we are also taking into account scoped names here, `duplicates` might have entries that are actually unique, but we still want them namespaced, so this needs to be taken into account in `update_dup_names`
            .chain(scoped_names_for_finding_duplicates.iter())
            .filter(|(_module_path, name2)| &name.1 == name2)
            .count()
            > 1
        {
            duplicates.push(Duplicate {
                namespace: Vec::<String>::new(),
                module_path: name.0.clone(),
                name: name.1.clone(),
                original_module_path: name.0.clone(),
            });
        }
    }
    // First add a single path segment to names that are duplicated by scoped items
    for dup in duplicates.iter_mut() {
        let is_scoped_name = scoped_names_for_finding_duplicates
            .iter()
            .any(|s| dup.name == s.1);
        if is_scoped_name {
            dup.namespace.insert(0, dup.module_path.pop().unwrap())
        }
    }
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);

    for dup in duplicates.iter_mut() {
        dup.namespace.push(dup.name.clone());
    }

    // resolve_use_stmts(&mut modules);

    // global_data_crate_path is use when reading module files eg global_data_crate_path = "../my_crate/" which is used to prepend "src/some_module/submodule.rs"
    let mut global_data = GlobalData::new(crate_path, duplicates.clone());
    global_data.modules = modules;

    // In extract_data() we record all module level item/fn/trait definitions/data in the `ModulData`. However, the types used in the definition might be paths to an item/fn that hasn't been parsed yet. This means we need to either:
    // 1. Only use syn for resolving types. Currently we use `get_path_without_namespacing()` to resolve paths which uses the parsed `ItemDefinition`s etc, we could update it/make a different version to work directly on syn data, but this seems like it would be inefficient.
    // 2. Do another pass. Leave the types empty in the definitions for `extract_data()` and then do another pass to populate them once we have all the item defs etc so `get_path_without_namespacing()` will work.
    // NOTE `get_path_without_namespacing()` shouldn't need full `ItemDefinition`s etc, it should only need the names of each item in each module. Given having partially completed `ItemDefinition`s etc will be a pain without using Option<T> or something (eg FnInfo has `return_type: RustType`, though could just use eg RustType::Todo or something), maybe we should update `get_path_without_namespacing()` to only use eg ModuleData.pub_definitions instead of `ItemDefinition`s... NO:
    // So `get_path_without_namespacing()` doesn't actually use `ItemDefinition`s etc, only ModuleData.pub_definitions etc...
    // So we can just entirely populate .item_definitions etc in the second pass, as parse_input... whatever is not using item_definitions...

    // So, we are using `parse_fn_input_or_field()` to parse: struct fields, and probably more stuff when finished like fn inputs and return, maybe trait bound?, etc
    // `parse_fn_input_or_field()` currently uses `ItemDefinition`s etc, which doesn't work because that is what we are in the process of creating... however the only thing it uses from the `ItemDefinition` is the names of the genereics ie:
    // let gen_arg_name = item_definition.generics[i].clone();
    //
    extract_data_populate_item_definitions(&mut global_data);

    global_data.transpiled_modules.push(JsModule {
        public: true,
        name: "web_prelude".to_string(),
        module_path: vec!["web_prelude".to_string()],
        stmts: Vec::new(),
    });
    let stmts = js_stmts_from_syn_items(
        prelude_items,
        &mut vec!["web_prelude".to_string()],
        &mut global_data,
    );

    global_data.transpiled_modules.push(JsModule {
        public: true,
        name: "crate".to_string(),
        module_path: vec!["crate".to_string()],
        stmts: Vec::new(),
    });

    // NOTE IMPORTANT item impls are populated in js_stmts_from_syn_items, which we don't run for web_prelude, which means create_element method is not found
    let stmts = js_stmts_from_syn_items(items, &mut vec!["crate".to_string()], &mut global_data);

    let stmts = if with_rust_types {
        push_rust_types(&global_data, stmts)
    } else {
        stmts
    };

    let crate_module = global_data
        .transpiled_modules
        .iter_mut()
        .find(|tm| tm.module_path == vec!["crate".to_string()])
        .unwrap();
    crate_module.stmts = stmts;

    update_classes(
        &mut global_data.transpiled_modules,
        &global_data.impl_blocks,
        &global_data.default_trait_impls_class_mapping,
        &global_data.default_trait_impls,
    );
    // dbg!(&global_data.transpiled_modules);

    // resolve paths to get canonical path to item
    // Update paths to use namespaced names and account for `use` statements
    // We want to find the module where the item is actually defined, not where it is mod, use, pub from other modules
    // Solution:
    // x First find all the items defined in each module
    // Then for each module, add maps of the names of the items that are mod/use'd to the use path
    // go through each module and reconcile each item with the use mapping, and that with the module path where the item is actually defined

    // Remember that use might only be `use`ing a module, and then completing the path to the actual item in the code. So the final step of reconciliation will always need make use of the actual paths/items in the code
    // dbg!(global_data.modules);

    // TODO can this not just be done automatically in JsModule.js_string() ??
    // add module name comments when there is more than 1 module
    if global_data.transpiled_modules.len() > 1 {
        for module in global_data
            .transpiled_modules
            .iter_mut()
            .filter(|m| m.module_path != ["web_prelude"])
        {
            module.stmts.insert(
                0,
                JsStmt::Comment(if module.module_path == ["crate"] {
                    "crate".to_string()
                } else {
                    module
                        .module_path
                        .iter()
                        .cloned()
                        .skip(1)
                        .collect::<Vec<_>>()
                        .join("::")
                }),
            );
        }
    }

    global_data.transpiled_modules.clone()
}

pub fn modules_to_string(modules: &Vec<JsModule>, run_main: bool) -> String {
    let mut module_strings = modules
        .iter()
        .map(|module| module.js_string())
        .collect::<Vec<_>>();
    if run_main {
        module_strings.push("main();".to_string());
    }
    module_strings.join("\n\n")
}

pub fn from_crate(crate_path: PathBuf, with_rust_types: bool, run_main: bool) -> String {
    let code = fs::read_to_string(crate_path.join("src").join("main.rs")).unwrap();
    let file = syn::parse_file(&code).unwrap();
    let items = file.items;

    // Crate path is eg "../for-testing/"
    let modules = process_items(items, Some(crate_path.clone()), with_rust_types);
    modules_to_string(&modules, run_main)
}

// Given every file *is* a module, and we concatenate all modules, including inline ones, into a single file, we should treat transpiling individual files *or* module blocks the same way
// Modules defined within a scope, eg a block, are not global and only accessible from that scope, but are treated the same way as other modules in that they are made global to the scope in which they are defined
pub fn from_file(code: &str, with_rust_types: bool) -> Vec<JsModule> {
    let file = syn::parse_file(code).unwrap();
    let items = file.items;

    process_items(items, None, with_rust_types)
}

pub fn from_block(code: &str, with_rust_types: bool) -> Vec<JsStmt> {
    // TODO should have a check to disallow use of `use` statement for `from_block` given we have no knowledge of the directory structure so can't lookup modules/crates in other files. Should web prelude be allowed?

    // let file = syn::parse_file(code).unwrap();
    let expr_block = syn::parse_str::<ExprBlock>(code).unwrap();

    // let mut names = Vec::new();
    let mut modules = Vec::new();
    modules.push(ModuleData {
        name: "crate".to_string(),
        parent_name: None,
        path: vec!["crate".to_string()],
        pub_definitions: Vec::new(),
        private_definitions: Vec::new(),
        pub_submodules: Vec::new(),
        private_submodules: Vec::new(),
        pub_use_mappings: Vec::new(),
        private_use_mappings: Vec::new(),
        resolved_mappings: Vec::new(),
        fn_info: Vec::new(),
        item_definitons: Vec::new(),
        trait_definitons: Vec::new(),
        consts: Vec::new(),
        items: Vec::new(),
    });
    let mut get_names_module_path = vec!["crate".to_string()];

    // let mut get_names_crate_path = crate_path.join("src/main.rs");
    // let mut get_names_crate_path = crate_path.clone();
    // extract_data(
    //     &file.items,
    //     None,
    //     &mut get_names_module_path,
    //     &mut names,
    //     &mut modules,
    // );

    // let mut duplicates = Vec::new();
    // for name in &names {
    //     if names
    //         .iter()
    //         .filter(|(module_path, name2)| &name.1 == name2)
    //         .collect::<Vec<_>>()
    //         .len()
    //         > 1
    //     {
    //         duplicates.push(Duplicate {
    //             namespace: Vec::<String>::new(),
    //             module_path: name.0.clone(),
    //             name: name.1.clone(),
    //             original_module_path: name.0.clone(),
    //         });
    //     }
    // }
    // update_dup_names(&mut duplicates);
    // update_dup_names(&mut duplicates);
    // update_dup_names(&mut duplicates);
    // update_dup_names(&mut duplicates);
    // update_dup_names(&mut duplicates);
    // update_dup_names(&mut duplicates);

    // for dup in duplicates.iter_mut() {
    //     dup.namespace.push(dup.name.clone());
    // }

    // resolve_use_stmts(&mut modules);

    let mut global_data = GlobalData::new(None, Vec::new());
    global_data.modules = modules;

    global_data.transpiled_modules.push(JsModule {
        public: true,
        name: "crate".to_string(),
        module_path: vec!["crate".to_string()],
        stmts: Vec::new(),
    });
    // let stmts = js_stmts_from_syn_items(
    //     file.items,
    //     true,
    //     &mut vec!["crate".to_string()],
    //     &mut global_data,
    //     &mut None,
    // );

    // It is better to parse this as an actual block expression and then just remove the braces/take the stmts within, because `handle_stmt()` will parse any items as being not module level, which means impls are added to the scope, which we don't have, so calling update_classes() tries to use module level impls which don't exist.
    // let mut stmts = expr_block
    //     .block
    //     .stmts
    //     .iter()
    //     .map(|stmt| handle_stmt(stmt, &mut global_data, &vec!["crate".to_string()]).0)
    //     .collect::<Vec<_>>();
    let (js_block, _rust_type) =
        handle_expr_block(&expr_block, &mut global_data, &vec!["crate".to_string()]);
    let stmts = match js_block {
        JsExpr::Block(js_stmts) => js_stmts,
        _ => todo!(),
    };

    let stmts = if with_rust_types {
        push_rust_types(&global_data, stmts)
    } else {
        stmts
    };

    let crate_module = global_data
        .transpiled_modules
        .iter_mut()
        .find(|tm| tm.module_path == vec!["crate".to_string()])
        .unwrap();
    crate_module.stmts = stmts;

    // update_classes(
    //     &mut global_data.transpiled_modules,
    //     &global_data.impl_items,
    //     &global_data.default_trait_impls_class_mapping,
    //     &global_data.default_trait_impls,
    // );
    // update_classes(
    //     &mut global_data.transpiled_modules,
    //     &global_data.impl_blocks,
    //     &global_data.default_trait_impls_class_mapping,
    //     &global_data.default_trait_impls,
    // );

    // and module name comments when there is more than 1 module
    if global_data.transpiled_modules.len() > 1 {
        for module in &mut global_data.transpiled_modules {
            // dbg!(&module);
            module.stmts.insert(
                0,
                JsStmt::Comment(if module.module_path == ["crate"] {
                    "crate".to_string()
                } else {
                    module
                        .module_path
                        .iter()
                        .cloned()
                        .skip(1)
                        .collect::<Vec<_>>()
                        .join("::")
                }),
            );
        }
    }

    global_data.transpiled_modules[0].stmts.clone()
}

// TODO combine this with from_file
pub fn from_module(code: &str, with_vec: bool) -> Vec<JsStmt> {
    let item_mod = syn::parse_str::<ItemMod>(code).unwrap();
    let items = item_mod.content.unwrap().1;
    let mut current_module = Vec::new();
    let mut global_data = GlobalData::new(None, Vec::new());
    js_stmts_from_syn_items(items, &mut current_module, &mut global_data)
}

pub fn from_fn(code: &str) -> Vec<JsStmt> {
    let item_fn = syn::parse_str::<ItemFn>(code).unwrap();

    let mut js_stmts = Vec::new();
    for stmt in &item_fn.block.stmts {
        let js_stmt = handle_stmt(stmt, &mut GlobalData::new(None, Vec::new()), &Vec::new()).0;
        js_stmts.push(js_stmt);
    }
    js_stmts
}

// pub fn from_block(code: &str) -> Vec<JsStmt> {
//     let expr_block = syn::parse_str::<ExprBlock>(code).unwrap();

//     let mut js_stmts = Vec::new();
//     for stmt in &expr_block.block.stmts {
//         let js_stmt = handle_stmt(
//             stmt,
//             &mut GlobalData::new(true, None, Vec::new()),
//             &vec!["crate".to_string()],
//         );
//         js_stmts.push(js_stmt);
//     }
//     js_stmts
// }

fn parse_fn_body_stmts(
    is_arrow_fn: bool,
    returns_non_mut_ref_val: bool,
    stmts: &Vec<Stmt>,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> (Vec<JsStmt>, RustType) {
    // let mut return_type = RustType::Todo;
    let mut js_stmts = Vec::new();
    // let (js_stmts, types): (Vec<_>, Vec<_>) = stmts
    //     .iter()
    //     .enumerate()
    //     .map(|(i, stmt)| )
    //     .unzip();

    // It is important to be able to generate no body arrow fns like `(x) => x + 1` eg for `.map()` etc but we need to be able to determine whether the resultant expression is suitable to fit in or should be within braces and thus require a return statement, which is not straightforward. Eg a call might be a single line depending on how many args/length of idents, a macro might be depending on what it expands/transpiles to, etc. Really we need to parse it, format it, then check whether it is a single line. We take a simplified approach here.
    let is_single_expr_return = if stmts.len() == 1 {
        match stmts.get(0).unwrap() {
            Stmt::Local(_) => false,
            Stmt::Item(_) => false,
            Stmt::Expr(expr, _) => match expr {
                Expr::Array(_) => true,
                Expr::Assign(_) => true,
                Expr::Async(_) => todo!(),
                Expr::Await(_) => true,
                Expr::Binary(_) => true,
                Expr::Call(_) => true,
                Expr::Cast(_) => true,
                Expr::Field(_) => true,
                // TODO should be true for if expressions that transpile to a (short) ternary
                Expr::If(_) => false,
                Expr::Index(_) => true,
                Expr::Lit(_) => true,
                Expr::Macro(_) => true,
                Expr::MethodCall(_) => true,
                Expr::Paren(_) => true,
                Expr::Path(_) => true,
                Expr::Range(_) => todo!(),
                Expr::Reference(_) => true,
                Expr::Repeat(_) => true,
                Expr::Struct(_) => true,
                Expr::Tuple(_) => true,
                Expr::Unary(_) => true,
                Expr::Unsafe(_) => todo!(),
                Expr::Verbatim(_) => todo!(),
                _ => false,
            },
            Stmt::Macro(_) => true,
        }
    } else {
        false
    };

    // TODO loads of duplication here and needs documenting why each special case is requried
    let mut return_type = None;
    for (i, stmt) in stmts.iter().enumerate() {
        // Manually set assignment var name for if expressions that are a return stmt
        if i == stmts.len() - 1 {
            match stmt {
                Stmt::Expr(expr, semi) => match expr {
                    Expr::If(expr_if) => {
                        if semi.is_some() {
                            let (stmt, type_) = handle_stmt(stmt, global_data, current_module);
                            js_stmts.push(stmt);
                            return_type = Some(type_);
                        } else {
                            // TODO should be using same code to parse Expr::If as elsewhere in code
                            let (condition, type_) =
                                handle_expr(&*expr_if.cond, global_data, current_module);
                            let condition = Box::new(condition);
                            let stmt = JsStmt::Expr(
                                JsExpr::If(JsIf {
                                    assignment: Some(LocalName::Single(
                                        "ifTempAssignment".to_string(),
                                    )),
                                    declare_var: true,
                                    condition,
                                    succeed: expr_if
                                        .then_branch
                                        .stmts
                                        .iter()
                                        .map(|stmt| {
                                            handle_stmt(stmt, global_data, current_module).0
                                        })
                                        .collect::<Vec<_>>(),
                                    fail: expr_if.else_branch.as_ref().map(|(_, expr)| {
                                        Box::new(handle_expr(&*expr, global_data, current_module).0)
                                    }),
                                }),
                                false,
                            );
                            js_stmts.push(stmt);
                            return_type = Some(type_);
                        }
                    }
                    Expr::Match(expr_match) => {
                        if semi.is_some() {
                            let (stmt, type_) = handle_stmt(stmt, global_data, current_module);
                            js_stmts.push(stmt);
                            return_type = Some(type_);
                        } else {
                            let (if_expr, type_) =
                                handle_expr_match(expr_match, true, global_data, current_module);
                            js_stmts.push(JsStmt::Expr(if_expr, true));
                            js_stmts.push(JsStmt::Expr(
                                JsExpr::Return(Box::new(JsExpr::Path(vec![
                                    "ifTempAssignment".to_string()
                                ]))),
                                true,
                            ));
                            return_type = Some(type_);
                        }
                    }
                    Expr::Path(expr_path)
                        if returns_non_mut_ref_val
                            && expr_path.path.segments.len() == 1
                            && semi.is_none() =>
                    {
                        // NOTE a len=1 path could also be a const or a fn
                        let var_name = expr_path.path.segments.first().unwrap().ident.to_string();
                        let var_info = global_data
                            .scopes
                            .iter()
                            .rev()
                            .find_map(|s| s.variables.iter().rev().find(|v| v.name == var_name))
                            .unwrap();
                        let mut js_var = JsExpr::Path(vec![var_name]);
                        if var_info.mut_ {
                            js_var = JsExpr::Field(Box::new(js_var), "inner".to_string())
                        }
                        let stmt = JsStmt::Expr(JsExpr::Return(Box::new(js_var)), true);
                        // Lookup path to get return type
                        // JsStmt::Expr(JsExpr::Return(Box::new(js_var)), true)
                        js_stmts.push(stmt);
                        return_type = Some(var_info.type_.clone());
                    }
                    // Expr::Unary(expr_unary) if returns_non_mut_ref_val && semi.is_none() => {
                    //     // if equivalent to JS primitive deref of mut/&mut number, string, or boolean, then call inner, else call copy (note we are only handling paths at the mo as we can find the types for them)
                    //     // TODO this logic and other stuff in this fn is duplicating stuff that should/does already exist in handle_expr
                    //     // The problem is we need to know `returns_non_mut_ref_val`?

                    //     let (expr, type_) =
                    //         handle_expr(&*expr_unary.expr, global_data, current_module);
                    //     // return_type = type_;

                    //     js_stmts.push(JsStmt::Expr(expr, true));
                    //     return_type = Some(type_);
                    //     // (JsStmt::Expr(expr, false), type_)
                    // }
                    _ => {
                        if semi.is_some() {
                            let (stmt, type_) = handle_stmt(stmt, global_data, current_module);
                            js_stmts.push(stmt);
                            return_type = Some(type_);
                        } else {
                            let (mut js_expr, type_) =
                                handle_expr(expr, global_data, current_module);
                            // Is the thing being returned a JS primative mut var or &mut (ie has a RustInteger wrapper)? in which case we need to get the inner value if `returns_non_mut_ref_val` is true

                            // TODO leave false for now until I clean up/refactor this code since this `is_js_primative_mut_var` should get caught be the Expr::Path branch
                            let is_js_primative_mut_var = false;
                            let is_js_primative_mut_ref = type_.is_mut_ref_of_js_primative();

                            if returns_non_mut_ref_val
                                && (is_js_primative_mut_var || is_js_primative_mut_ref)
                            {
                                js_expr = JsExpr::Field(Box::new(js_expr), "inner".to_string());
                            }

                            let return_expr = if is_arrow_fn && is_single_expr_return {
                                JsStmt::Expr(js_expr, true)
                            } else {
                                JsStmt::Expr(JsExpr::Return(Box::new(js_expr)), true)
                            };
                            js_stmts.push(return_expr);
                            return_type = Some(type_);
                        }
                    }
                },
                _ => {
                    let (stmt, type_) = handle_stmt(stmt, global_data, current_module);
                    js_stmts.push(stmt);
                    return_type = Some(type_);
                }
            }
        } else {
            let (stmt, type_) = handle_stmt(stmt, global_data, current_module);
            js_stmts.push(stmt);
            return_type = Some(type_);
        }
    }

    if stmts.len() == 0 {
        (Vec::new(), RustType::Unit)
    } else {
        (js_stmts, return_type.unwrap())
    }
}

fn hardcoded_conversions(expr_path: &ExprPath, args: Vec<JsExpr>) -> Option<(JsExpr, RustType)> {
    let segments = expr_path
        .path
        .segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect::<Vec<_>>();

    if segments.last().unwrap() == "fetch2" {
        // TODO improve this code
        Some((
            JsExpr::FnCall(Box::new(JsExpr::Path(vec!["fetch".to_string()])), args),
            RustType::Todo,
        ))
    } else if segments.last().unwrap() == "stringify" {
        Some((
            JsExpr::FnCall(
                Box::new(JsExpr::Path(vec![
                    "JSON".to_string(),
                    "stringify".to_string(),
                ])),
                args,
            ),
            RustType::Todo,
        ))
    } else if segments.len() == 2 && segments[0] == "Json" && segments[1] == "parse" {
        Some((
            JsExpr::FnCall(
                Box::new(JsExpr::Path(vec!["JSON".to_string(), "parse".to_string()])),
                args,
            ),
            RustType::Todo,
        ))
    } else if segments.len() == 2 && segments[0] == "Date" && segments[1] == "from_iso_string" {
        Some((JsExpr::New(vec!["Date".to_string()], args), RustType::Todo))
    } else if segments.len() == 2
        && segments[0] == "Document"
        && segments[1] == "query_selector_body"
    {
        Some((
            JsExpr::FnCall(
                Box::new(JsExpr::Path(vec![
                    "document".to_string(),
                    "querySelector".to_string(),
                ])),
                vec![JsExpr::LitStr("body".to_string())],
            ),
            RustType::Todo,
        ))
    } else if segments.len() == 2
        && segments[0] == "Document"
        && segments[1] == "create_element_div"
    {
        Some((
            JsExpr::FnCall(
                Box::new(JsExpr::Path(vec![
                    "document".to_string(),
                    "createElement".to_string(),
                ])),
                vec![JsExpr::LitStr("div".to_string())],
            ),
            RustType::Todo,
        ))
    } else {
        None
    }
}

// NOTE I'm pretty sure get_path will currently convert something like `super::bar::baz::MyEnum::MyVariant` to `foo::bar::baz::MyEnum::MyVariant` ideally we would return the module path and item separately, but for now can just look up the item afterwards
// NOTE the segs returned might be namespaced with "__". This is something I plan to change and do the namespacing later
// So we have a path like foo::bar::baz()
// The algorithm for finding the full module path to the item is:
// Iif segs[0] is crate, super, or self, then jump to that module
// else
// Look to see if segs[0] is defined in any other the parent scopes
// else
// Look to see if segs[0] is defined at the module level
// else
// Look to see if segs[0] is a child module
// else
// Look to see if segs[0] is used

// TODO:
// handle mixed paths eg submodule -> use
// make sure `mod` being pub/private is taken into account - this would only be for use paths since mod is always public in the file it is called from (parent), so would happen in the `use` resolving step

// Take a path segs like foo::my_func(), and finds the absolute path to the item eg crate::bar::foo::my_func()
// Actually it should find the path relative to the seed path ie current_module, which is why it is useful to use recursively and for resolving use paths???
// What happens if the path is to a scoped item, or a variable (ie not an item definition)? We return the path of the item/var, which I believe in all cases must be a 0 length path/Vec<String>?
fn get_path_old(
    look_for_scoped_vars: bool,
    use_private_items: bool,
    // So we know whether allow segs to simply be somthing in an outer scope
    module_level_items_only: bool,
    module: &ModuleData,
    mut segs: Vec<String>,
    global_data: &GlobalData,
    current_module: &Vec<String>,
    // Only used to determine if current module is
    original_module: &Vec<String>,
) -> Vec<String> {
    // TODO All parent modules are visible/public to their desecendants. When parents are accessed via `super`, it is easy the flag `use_private_items = true`. However when modules are accessed via `crate:: ...` I don't think there is any way to know whether the path leads a module which is a parent of (or is) the original module (ie so should be public), so we need to do this check. No - even if we access an item via `crate`, once we then visit a submodule, `use_private_items` get set to false, the problem is actually that sometimes we will want it to be true, when crate::submodule is actually a parent of the original module. So really we should just keep `is_parent_or_same_module` but a more efficient approach is to use `use_private_items` for crate, super, self, etc, then only calculate `is_parent_or_same_module` for submodule and pass as use_private_items
    let is_parent_or_same_module = if original_module.len() >= current_module.len() {
        current_module
            .iter()
            .enumerate()
            .all(|(i, current_module)| current_module == &original_module[i])
    } else {
        false
    };

    let item_defined_in_module =
        module.item_defined_in_module(use_private_items || is_parent_or_same_module, &segs[0]);

    // Whilst the immediate child modules are public to all modules (not their items, but the module itself), we might not actually be accessing it directly from the parent eg `foo::bar::baz()` and `bar` is not `pub`
    let path_starts_with_sub_module =
        module.path_starts_with_sub_module(use_private_items || is_parent_or_same_module, &segs[0]);

    // We are looking for the origin of some name so we want to compare that name with the item the use statement is importing
    // If we have a match then use want to add the use path to the path of the current module, eg:
    // current_module::crate::foo:item - we can overwrite the current module with "crate"
    // current_module::super::foo:item - we can pop the last seg of the current module
    // current_module::submodule::foo::item - add the submodule to the current module
    // current_module::another_used_item::item - we need to get the path of this subsequent used item in the same way described here, so we are recursing, then return to the original use path and carry on adding the rest of the segments
    let mut use_mappings = module.pub_use_mappings.iter();
    let matched_use_mapping = if use_private_items || is_parent_or_same_module {
        use_mappings
            .chain(module.private_use_mappings.iter())
            .find(|use_mapping| use_mapping.0 == segs[0])
    } else {
        use_mappings.find(|use_mapping| use_mapping.0 == segs[0])
    };
    // let path_starts_with_a_used_item_or_mod = module
    //     .resolved_mappings
    //     .iter()
    //     .find(|use_mapping| use_mapping.0 == segs[0]);

    let is_scoped = global_data.scopes.iter().rev().any(|scope| {
        let is_func = scope.fns.iter().any(|func| func.ident == segs[0]);
        let is_item_def = scope
            .item_definitons
            .iter()
            .any(|item_def| item_def.ident == segs[0]);

        // self could be and instance or module path ie `fn foo(&self) { self }` or `self::MyStruct`. I can't think of any situations where a module path can
        let is_var = scope.variables.iter().any(|var| var.name == segs[0])
            && segs.len() == 1
            && look_for_scoped_vars;

        // A scoped item must be the first element in the segs, ie in the original module so we need `current_module == original_module`
        (is_func || is_item_def || is_var) && current_module == original_module
    });

    // dbg!(&global_data.scopes);

    // TODO not sure why we need use_private_items here
    // if use_private_items && is_scoped {
    if is_scoped {
        // Variables and scoped items
        // Need to handle scoped vars and items first, otherwise when handling as module paths, we would always first have to check if the path is a scoped var/item

        // If we are returning a scoped var/item, no recursion should have occured so we should be in the same module
        assert!(current_module == original_module);
        segs
    } else if item_defined_in_module {
        // Path starts with an item defined in the module

        // Check whether it is not globally unique and so has been namespaced
        // TODO surely the global namespacing could and should happen just before writing the JS? It just get's in the way if it is already done at this point and we could just be using the module paths to differentiate
        if let Some(dup) = global_data
            .duplicates
            .iter()
            .find(|dup| dup.name == segs[0] && &dup.original_module_path == current_module)
        {
            segs[0] = dup
                .namespace
                .iter()
                .map(|seg| camel(seg))
                .collect::<Vec<_>>()
                .join("__");
        }

        // dbg!("item defined in module");

        segs
    } else if segs[0] == "super" {
        // TODO if a module level item name is shadowed by a item in a fn scope, then module level item needs to be namespaced
        segs.remove(0);

        let mut current_module = current_module.clone();
        current_module.pop();

        let module = global_data
            .modules
            .iter()
            .find(|module| module.path == current_module)
            .unwrap();

        // dbg!("in super");
        get_path_old(
            false,
            true,
            true,
            module,
            segs,
            global_data,
            &current_module,
            original_module,
        )
    } else if segs[0] == "self" {
        // NOTE private items are still accessible from the module via self
        segs.remove(0);

        // I believe this works because the only effect of self is to look for the item only at the module level, rather than up through the fn scopes first, so get_path without the self and `in_same_module = false` achieves this, including handling any subsequent `super`s
        // TODO problem is that we are conflating `in_same_module` with pub/private
        // dbg!("in self");
        get_path_old(
            false,
            true,
            true,
            module,
            segs,
            global_data,
            &current_module,
            original_module,
        )
    } else if segs[0] == "crate" {
        let current_module = vec!["crate".to_string()];
        let module = global_data
            .modules
            .iter()
            .find(|module| module.path == current_module)
            .unwrap();

        // TODO descendants can access private items, so it depends whether the module trying to access the item is a descendant of the module it item is in, so need to keep track of the original call site?

        // dbg!("in crate");
        segs.remove(0);

        // NOTE all modules are desecendants of crate so all items in crate are visible/public
        get_path_old(
            false,
            true,
            true,
            module,
            segs,
            global_data,
            &current_module,
            original_module,
        )
    } else if path_starts_with_sub_module {
        // Path starts with a submodule of the current module
        let mut submodule_path = current_module.clone();
        submodule_path.push(segs[0].clone());

        let submodule = global_data
            .modules
            .iter()
            .find(|submodule| submodule.path == submodule_path)
            .unwrap();

        // If we are going to be looking within the submodule that the current path starts with, we should remove it from the path since get_path() doesn't expect paths to a module to start with their name since this is not valid Rust, and self must be used instead.
        segs.remove(0);

        // dbg!("Path starts with a submodule of the current module");
        get_path_old(
            false,
            false,
            true,
            submodule,
            segs,
            global_data,
            &submodule_path,
            original_module,
        )
    } else if let Some(use_mapping) = matched_use_mapping {
        let mut use_segs = use_mapping.1.clone();
        use_segs.push(use_mapping.0.clone());
        segs.remove(0);
        use_segs.extend(segs);
        let mut segs = get_path_old(
            false,
            true,
            true,
            module,
            use_segs,
            global_data,
            current_module,
            original_module,
        );

        if let Some(dup) = global_data
            .duplicates
            .iter()
            .find(|dup| dup.name == use_mapping.0 && dup.original_module_path == use_mapping.1)
        {
            // If the item has been namespaced, we need to replace it with the namespace
            segs[0] = dup
                .namespace
                .iter()
                .map(|seg| camel(seg))
                .collect::<Vec<_>>()
                .join("__");
            segs
        } else {
            // If the item has not been namespaced, we don't need to do anything
            segs
        }
        // TODO `this` is a JS ident, we want to be dealing with Rust idents at this point
    } else if segs.len() == 1 && segs[0] == "this" {
        segs
    } else if use_private_items {
        // Variables and scoped items

        // If none of the above conditions are met then assume the path refers to an item or variable in an enclosing scope within this module, not an item at the top level of a module, so simply return the path
        // dbg!("in private items");

        // Since this is the path to a scoped item or var, the path must be length 1
        // dbg!(&segs);
        assert!(segs.len() == 1);
        segs
    } else {
        dbg!(module);
        dbg!(current_module);
        dbg!(segs);
        panic!()
    }
}

// TODO this is needed because we want to combine the use_mapping Vec<String> with segs Vec<PathSegment>, but it might be better to just store use_mapping as Vec<PathSegment>.
#[derive(Debug, Clone)]
pub struct RustPathSegment {
    ident: String,
    turbofish: Vec<RustType>,
}

// TODO ideally test tracing output in get_path test cases to ensure the expect code path is being taken??
// TODO need to make sure this looks up traits as well as other items
/// -> (current module (during recursion)/item module path (upon final return), found item path, is scoped item/var/func)
///
/// TODO maybe should return Option<Vec<String>> for the module path to make it consistent with the rest of the codebase, but just returning a bool is cleaner
fn get_path(
    look_for_scoped_vars: bool,
    // TODO can we combine this with `look_for_scoped_vars`?
    look_for_scoped_items: bool,
    use_private_items: bool,
    mut segs: Vec<RustPathSegment>,
    // TODO replace GlobalData with `.modules` and `.scopes` to making setting up test cases easier
    global_data: &GlobalData,
    current_mod: &Vec<String>,
    // Only used to determine if current module is the original module
    orig_mod: &Vec<String>,
) -> (Vec<String>, Vec<RustPathSegment>, bool) {
    debug!(segs = ?segs, "get_path_without_namespacing");

    // dbg!(&current_mod);
    // dbg!(&segs);

    // TODO I don't think we need to pass in the module `ModuleData` if we are already passing the `current_module` module path we can just use that to look it up each time, which might be less efficient since we shouldn't need to lookup the module if we haven't changed modules (though I think we are pretty much always changing modules except for use statements?), but we definitely don't want to pass in both. Maybe only pass in `module: &ModuleData` and not `current_module`
    // assert!(current_module == &module.path);
    let module = global_data
        .modules
        .iter()
        .find(|m| &m.path == current_mod)
        .unwrap();
    // dbg!(&module);

    let is_parent_or_same_module = if orig_mod.len() >= current_mod.len() {
        current_mod
            .iter()
            .enumerate()
            .all(|(i, current_module)| current_module == &orig_mod[i])
    } else {
        false
    };

    let item_defined_in_module = module.item_defined_in_module(
        use_private_items || is_parent_or_same_module,
        &segs[0].ident,
    );
    // dbg!(&item_defined_in_module);

    let path_starts_with_sub_module = module.path_starts_with_sub_module(
        use_private_items || is_parent_or_same_module,
        &segs[0].ident,
    );

    // TODO only look through transparent scopes
    let scoped_use_mapping = global_data
        .scopes
        .iter()
        .rev()
        .find_map(|s| s.use_mappings.iter().find(|u| u.0 == segs[0].ident));
    let mut use_mappings = module.pub_use_mappings.iter();
    let matched_use_mapping = if scoped_use_mapping.is_some() {
        scoped_use_mapping
    } else if use_private_items || is_parent_or_same_module {
        use_mappings
            .chain(module.private_use_mappings.iter())
            .find(|use_mapping| use_mapping.0 == segs[0].ident)
    } else {
        use_mappings.find(|use_mapping| use_mapping.0 == segs[0].ident)
    };
    // dbg!(matched_use_mapping);

    // TODO can module shadow external crate names? In which case we need to look for modules first? I think we do this implicitly based on the order of the if statements below?
    // TODO actually look up external crates in Cargo.toml
    let external_crate_names = vec!["web_prelude"];
    let path_is_external_crate = external_crate_names.iter().any(|cn| cn == &segs[0].ident);

    let is_scoped = global_data.scopes.iter().rev().any(|scope| {
        let is_func =
            look_for_scoped_items && scope.fns.iter().any(|func| func.ident == segs[0].ident);
        let is_item_def = look_for_scoped_items
            && scope
                .item_definitons
                .iter()
                .any(|item_def| item_def.ident == segs[0].ident);
        // TODO IMPORTANT
        // We cannot have a scoped item/fn definition with the same ident as a module, but we can have a scoped *var* with the same ident as a module/item/fn. I think Rust just chooses which one to use based on the context eg foo::bar must be an item/module, foo.bar() must be an instance/var, etc. To follow this approach would mean we need more context for this fn.
        // eg this is aloud:
        // use tracing;
        // let tracing = "ohno";
        // As far as I am aware the only common context in which a path might have length=1 is a use statement, which doesn't use this fn to resolve the path (though it probably should given we have to follow the same crate/self/super logic?) so for now just assume that if we match a scoped var name and the length is 1, then return the scoped var, even though technically it could be a module/item/fn eg this is valid: *NO* what about a simple `Foo {}` which is a path with length 1 where we could also have `let Foo = 5;`, which would make it impossible to decide which to return, eg module level struct (Some("crate"), ["Foo"]) vs scoped var (None, ["Foo"]).**
        // struct foo {}
        // fn main() {
        //     let foo = 5;
        // }
        // For scoped modules/items/fns there is currently no difference anyway since we currently just return Vec<RustPathSegment> regardless.
        // The main problem is the above example. However, the below is not valid which I believe demonstrates that it the ident must be unambigious if it can be used as eg a fn argument, and so it is indeed the context of where the path is being used eg `bar(foo);`, `let bar = foo;`, `foo {}`, etc which determines which thing to use. I think it will be non trivial to pass handle handle the different contexts to this fn.
        // struct foo;
        // fn main() {
        //     let foo = 5;
        // }
        // ** The distinction is between where the site of the path expects a definition (eg fn input type) and instances (eg assign to a variable). A slight complication is that both of the below are valid, just not simultaneously, but it still means that a path passed the the rhs of an assignment could be and instance *or* a definition, so we need to look for other if one doesn't exist, **but *only* if we have `struct foo;` and not `struct foo {}` because for the latter we *are* allowed both idents in scope, so need to ensure we choose the scoped var, in this case. This is as apposed to say the type of a fn input where we can always be sure to not look for scoped vars, only any items/fns.
        // let foo = 5;
        // let which = foo;
        // ...
        // struct foo;
        // let which = foo;
        // So I think maybe the trick is to pass an argument to this fn to say wether we should be considering vars and follow these rules:
        // 1. If including vars (eg simple 1-len path assignment) then look for a var first, else look for a struct, this way we will catch assigning `struct foo;` because a `foo` var can't exist in this case, and for `struct Foo {}` we will correctly pick the var first.
        // 2. If not inlcluding vars we simply don't have to look for vars.
        // Don't need both can just always do step 1?

        // self could be and instance or module path ie `fn foo(&self) { self }` or `self::MyStruct`. I can't think of any situations where a module path can
        let is_var = scope.variables.iter().any(|var| var.name == segs[0].ident)
            && segs.len() == 1
            && look_for_scoped_vars;

        // A scoped item must be the first element in the segs, ie in the original module so we need `current_module == original_module`
        // TODO I don't think `&& current_mod == orig_mod` is necessary given look_for_scoped_vars and look_for_scoped_items
        (is_func || is_item_def || is_var) && current_mod == orig_mod
    });

    // dbg!(&global_data.scopes);

    // TODO not sure why we need use_private_items here
    // if use_private_items && is_scoped {
    if is_scoped {
        // Variables and scoped items
        // Need to handle scoped vars and items first, otherwise when handling as module paths, we would always first have to check if the path is a scoped var/item

        // If we are returning a scoped var/item, no recursion should have occured so we should be in the same module
        assert!(current_mod == orig_mod);
        (current_mod.clone(), segs, true)
    } else if item_defined_in_module {
        (current_mod.clone(), segs, false)
    } else if segs[0].ident == "super" {
        // TODO if a module level item name is shadowed by an item in a fn scope, then module level item needs to be namespaced
        segs.remove(0);

        let mut current_module = current_mod.clone();
        current_module.pop();

        get_path(
            false,
            false,
            true,
            segs,
            global_data,
            &current_module,
            orig_mod,
        )
    } else if segs[0].ident == "self" {
        // NOTE private items are still accessible from the module via self
        segs.remove(0);

        get_path(
            false,
            false,
            true,
            segs,
            global_data,
            &current_mod,
            orig_mod,
        )
    } else if segs[0].ident == "crate" {
        let current_module = vec!["crate".to_string()];

        segs.remove(0);

        get_path(
            false,
            false,
            true,
            segs,
            global_data,
            &current_module,
            orig_mod,
        )
    } else if path_starts_with_sub_module {
        // Path starts with a submodule of the current module
        let mut submod_path = current_mod.clone();
        submod_path.push(segs[0].ident.to_string());

        segs.remove(0);

        get_path(
            false,
            false,
            false,
            segs,
            global_data,
            &submod_path,
            orig_mod,
        )
    } else if let Some(use_mapping) = matched_use_mapping {
        // Use mappings the resolved path for each item/module "imported" into the module with a use statement. eg a module containing
        // `use super::super::some_module::another_module;` will have a use mapping recorded of eg ("another_module", ["crate", "top_module", "some_module"])
        // So say we have a path like `another_module::MyStruct;`, then we will match this use mapping and the below code combines the path from the mapping and the current "segs" to make `"crate", "top_module", "some_module", "another_module", "MyStruct";`
        // What if we have a path like `another_module::yet_another_module::MyStruct;`??

        // TODO I think we need to set the current_module to use_mapping.1, remove this from segs (ie just not add it), and then we can just start from that module in the next get_path iteration?? NO That causes tests to fail

        let mut use_segs = use_mapping.1.clone();
        use_segs.push(use_mapping.0.clone());
        // TODO IMPORTANT seems like we are not correctly populating turbofish here
        let mut use_segs = use_segs
            .into_iter()
            .map(|s| RustPathSegment {
                ident: s,
                turbofish: Vec::new(),
            })
            .collect::<Vec<_>>();
        segs.remove(0);
        use_segs.extend(segs);

        // TODO IMPORTANT for a `use` statement for a third party crate, we need to set the `current_module` accordingly. I think it is fine to just use ["name_of_crate", "module_in_crate", etc].

        // TODO do we not need to update the current module if the use path/mapping has taken us to a new module?? write some tests NO because the use mapping just provides an absolute path to the module/item that is used which is of course valid from the current module. The important part is that the get_path iteration after this `matched_use_mapping` iteration

        // let new_mod = if use_segs[0].ident == "crate" {
        //     // current_mod
        //     use_mapping.1.clone()
        // } else if external_crate_names
        //     .iter()
        //     .any(|cn| cn == &use_segs[0].ident)
        // {
        //     use_mapping.1.clone()
        // } else {
        //     panic!()
        // };

        get_path(
            false,
            false,
            true,
            use_segs,
            global_data,
            // &new_mod,
            current_mod,
            // &use_mapping.1.clone(),
            orig_mod,
        )
    // } else if segs.len() == 1 && segs[0] == "this" {
    //     segs
    } else if path_is_external_crate {
        // } else if false {
        // TODO need to update current_mod

        // Handle equivalently to segs[0] == "crate"
        let crate_name = segs.remove(0);
        let current_module = [crate_name.ident].to_vec();

        get_path(
            false,
            false,
            true,
            segs,
            global_data,
            &current_module,
            orig_mod,
        )
    } else {
        // Handle third party crates
        // TODO lookup available crates in Cargo.toml
        // if segs[0..3].iter().map(|s| &s.ident).collect::<Vec<_>>()
        //     == ["ravascript", "prelude", "web"]
        // {
        //     return (
        //         vec!["prelude_special_case".to_string()],
        //         segs[3..].to_vec(),
        //         false,
        //     );
        // }

        // If we can't find the ident anywhere, the only remaining possibility is that we have a prelude type
        assert_eq!(current_mod, orig_mod);
        assert_eq!(segs.len(), 1);
        let seg = &segs[0];
        if seg.ident == "i32" || seg.ident == "String" || seg.ident == "str" {
            // TODO properly encode "prelude_special_case" in a type rather than a String
            (vec!["prelude_special_case".to_string()], segs, false)
        } else {
            // dbg!(module);
            dbg!(current_mod);
            dbg!(segs);
            panic!()
        }
    }
}

// return type for `handle_expr_path` because the path might not comprise a full expression/type, ie a tuple struct or enum variant that has args so requires being called
#[derive(Debug, Clone)]
pub enum PartialRustType {
    /// This is only used for tuple struct instantiation since normal struct instantiation are parsed to Expr::Struct and so can be directly evaluated to a struct instance, whereas a tuple struct instantiation is parsed as an ExprCall. Ok but Expr::Struct still has a `.path` `Path` field which we want to be able to parse/handle with the same handle_expr code, so now this can also be the path of a Expr::Struct
    ///
    /// So we are assuming that *all* cases where we have an Expr::Path and the final segment is a struct ident, it must be a tuple struct
    ///
    /// (type params, module path, name) module path is None for scoped structs
    StructIdent(Vec<RustTypeParam>, Option<Vec<String>>, String),
    /// This is only used for instantiation of enum variants with args which are parsed as an ExprCall, since normal enum variant instantiation are simply evaluated directly to an enum instance.
    /// Note we need to record type params because we might be parsing something like the `MyGenericEnum::<i32>::MyVariant` portion of `MyGenericEnum::<i32>::MyVariant("hi")` where the *enum definition* has had generics resolved
    ///
    /// (type params, module path, enum name, variant name) module path is None for scoped structs
    EnumVariantIdent(Vec<RustTypeParam>, Option<Vec<String>>, String, String),
    RustType(RustType),
}

/// For checking whether a struct item definition (possibly with resolved type params) matches the target type of a non-trait impl. Note this is not a simple equals since a Foo<i32> item matches a Foo<T> impl.
fn struct_or_enum_types_match(
    target_type: &RustType,
    item_generics: &Vec<RustTypeParam>,
    item_module_path: &Option<Vec<String>>,
    item_name: &String,
) -> bool {
    // match impl_block.target {
    match target_type {
        RustType::StructOrEnum(target_type_params, target_module_path, target_name) => {
            // Check that either:
            // 1. for each of the the path's type params, either the equivalent impl target's type param is generic or the path type param is concrete and matches the impl target's concrete type param
            // 2. the struct is not generic, so no type params
            assert!(item_generics.len() == target_type_params.len());

            let type_params_match = item_generics.iter().zip(target_type_params.iter()).all(
                |(struct_type_param, impl_target_type_params)| {
                    // Check both param lists are ordered the same way (ie the names match)
                    assert!(struct_type_param.name == impl_target_type_params.name);
                    match (&struct_type_param.type_, &impl_target_type_params.type_) {
                        (RustTypeParamValue::Unresolved, RustTypeParamValue::Unresolved) => true,
                        (RustTypeParamValue::Unresolved, RustTypeParamValue::RustType(_)) => false,
                        (RustTypeParamValue::RustType(_), RustTypeParamValue::Unresolved) => true,
                        (
                            RustTypeParamValue::RustType(struct_rust_type),
                            RustTypeParamValue::RustType(impl_target_rust_type),
                        ) => {
                            // TODO We might have nested generics eg a Foo<Bar<Baz<etc>>> concrete type, so need recursively do this checking until we find a type which has no generics
                            match (&**struct_rust_type, &**impl_target_rust_type) {
                                (RustType::NotAllowed, RustType::NotAllowed) => true,
                                (RustType::Unknown, RustType::Unknown) => true,
                                (RustType::Todo, RustType::Todo) => true,
                                (RustType::ParentItem, RustType::ParentItem) => true,
                                (RustType::Unit, RustType::Unit) => true,
                                (RustType::Never, RustType::Never) => true,
                                (RustType::ImplTrait(_), RustType::ImplTrait(_)) => true,
                                (RustType::TypeParam(_), RustType::TypeParam(_)) => true,
                                (RustType::I32, RustType::I32) => true,
                                (RustType::F32, RustType::F32) => true,
                                (RustType::Bool, RustType::Bool) => true,
                                (RustType::String, RustType::String) => true,
                                (RustType::Option(_), RustType::Option(_)) => true,
                                (RustType::Result(_), RustType::Result(_)) => true,
                                (
                                    RustType::StructOrEnum(_, _, _),
                                    RustType::StructOrEnum(_, _, _),
                                ) => true,
                                (RustType::Vec(_), RustType::Vec(_)) => true,
                                (RustType::Array(_), RustType::Array(_)) => true,
                                (RustType::Tuple(_), RustType::Tuple(_)) => true,
                                (RustType::UserType(_, _), RustType::UserType(_, _)) => true,
                                (RustType::MutRef(_), RustType::MutRef(_)) => true,
                                (RustType::Ref(_), RustType::Ref(_)) => true,
                                (RustType::Fn(_, _, _, _), RustType::Fn(_, _, _, _)) => true,
                                (_, _) => false,
                            }
                        }
                    }
                },
            );

            type_params_match && item_name == target_name && item_module_path == target_module_path
        }
        _ => false,
    }
}

/// TODO This seems to be specifically for handling the case that `if segs_copy_item_path.len() == 1`. This fn and that branch that calls it needs cleaning up.
/// Assumes that exactly 1 of var, func, or item_def is Some() and the rest are None
/// -> (partial, is_mut)
fn found_item_to_partial_rust_type(
    item_path: &RustPathSegment,
    var: Option<&ScopedVar>,
    func: Option<&FnInfo>,
    item_def: Option<&ItemDefinition>,
    const_def: Option<&ConstDef>,
    module_path: Option<Vec<String>>,
) -> (PartialRustType, bool) {
    debug!(item_path = ?item_path, var = ?var, func = ?func, item_def = ?item_def, module_path = ?module_path, "found_item_to_partial_rust_type");
    if let Some(var) = var {
        // This branch is obviously only possible for scoped paths since we can't have module level vars
        (PartialRustType::RustType(var.type_.clone()), var.mut_)
    } else if let Some(fn_info) = func {
        // If turbofish exists on item path segment then use that for type params, otherwise use the unresolved params defined on the fn definition
        let fn_generics = if item_path.turbofish.len() > 0 {
            item_path
                .turbofish
                .iter()
                .enumerate()
                .map(|(i, g)| RustTypeParam {
                    name: fn_info.generics[i].clone(),
                    type_: RustTypeParamValue::RustType(Box::new(g.clone())),
                })
                .collect::<Vec<_>>()
        } else {
            fn_info
                .generics
                .iter()
                .map(|g| RustTypeParam {
                    name: g.clone(),
                    type_: RustTypeParamValue::Unresolved,
                })
                .collect::<Vec<_>>()
        };
        (
            PartialRustType::RustType(RustType::Fn(
                None,
                fn_generics,
                module_path,
                RustTypeFnType::Standalone(item_path.ident.clone()),
            )),
            false,
        )
    } else if let Some(item_def) = item_def {
        // If turbofish exists on item path segment then use that for type params, otherwise use the unresolved params defined on the item definition
        let item_generics = if item_path.turbofish.len() > 0 {
            item_path
                .turbofish
                .iter()
                .enumerate()
                .map(|(i, g)| RustTypeParam {
                    name: item_def.generics[i].clone(),
                    type_: RustTypeParamValue::RustType(Box::new(g.clone())),
                })
                .collect::<Vec<_>>()
        } else {
            item_def
                .generics
                .iter()
                .map(|g| RustTypeParam {
                    name: g.clone(),
                    type_: RustTypeParamValue::Unresolved,
                })
                .collect::<Vec<_>>()
        };
        match &item_def.struct_or_enum_info {
            StructOrEnumDefitionInfo::Struct(struct_definition_info) => {
                // So we are assuming that *all* cases where we have an Expr::Path and the final segment is a struct ident, it must be a tuple struct??? Could also be an expr_struct.path
                (
                    PartialRustType::StructIdent(
                        item_generics,
                        module_path,
                        item_path.ident.clone(),
                    ),
                    false,
                )
            }
            StructOrEnumDefitionInfo::Enum(enum_definition_info) => {
                // So we are assuming you can't have a path where the final segment is an enum ident
                panic!()
            }
        }
    } else if let Some(const_def) = const_def {
        (PartialRustType::RustType(const_def.type_.clone()), false)
    } else {
        // dbg!(segs_copy);
        todo!()
    }
}

pub fn generate_js_from_module(js: impl ToString) -> String {
    from_module(js.to_string().as_str(), false)
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn format_js(js: impl ToString) -> String {
    let parse = biome_js_parser::parse_script(js.to_string().as_str(), JsParserOptions::default());
    let stmt = parse.syntax().children().nth(1).unwrap();
    let opts = JsFormatOptions::new(JsFileSource::default())
        // .with_indent_width(IndentWidth::from(1))
        .with_indent_style(IndentStyle::Space);
    let formatted_js = biome_formatter::format_node(&stmt, JsFormatLanguage::new(opts)).unwrap();
    formatted_js.print().unwrap().as_code().to_string()
}
