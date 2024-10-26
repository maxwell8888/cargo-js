use proc_macro2::TokenStream;
use quote::quote;
use std::{fs, path::PathBuf};
use syn::{
    AngleBracketedGenericArguments, Attribute, BinOp, BoundLifetimes, Expr, ExprYield,
    GenericParam, Generics, Ident, ImplItem, Item, ItemImpl, ItemUse, Label, Lifetime, Lit, Macro,
    Member, Meta, Pat, QSelf, RangeLimits, ReturnType, Stmt, TraitItem, Type, TypeParamBound, UnOp,
    UseTree, Visibility,
};
use syn::{ImplItemFn, ItemConst, ItemEnum, ItemFn, ItemStruct, ItemTrait, Signature, TraitItemFn};
use tracing::debug;

use crate::update_item_definitions::{ItemDefRc, RustTypeParam, RustTypeParamValue};
use crate::{update_item_definitions::ItemDef, RustPathSegment, RUST_PRELUDE_MODULE_PATH};

// Having "crate" in the module path is useful for representing that the top level module is indeed a module, and for giving it a name that can be looked up in the list. However, it is annoying for eg using the path to create a filepath from
// TODO crate_path might use hiphens instead of underscore as a word seperator, so need to ensure it is only used for file paths, and not module paths
// IMPORTANT TODO need to check for "crate" *and* "my_external_crate", and also use the corrent `crate_path`
// TODO I believe the `pub` keyword for scoped `use` statements is irrelevant/redundant given that idents from scoped `use` statements aren't visible outside the scope. The only time the are relevant is if there is also a *scoped* module inside the same scope, but this seems pretty niche so we will not handle this case for now.

// Returns a Vec of item (struct/enum/trait/const) defs, and a nested/AST of `ItemV1`s where a ItemV1 struct/enum/trait/const is simply an index into the Vec.
// NOTE because a FnInfo item itself can contain other items, including mods etc, it is not possible to keep struct/enum/trait/const's in a Vec, and the other items in a tree with indexes into the Vec
// I think the only option is to either keep the tree and the indexes must instead be a Vec of indexes (requiring allocation and keeping track of the indexes of parents), of keep everything in a flat structure. This means we need to be able to identify the crate level items somehow, eg by putting them in a RustMod

// When called for a top level module like a crate, the returned ItemRef's are the top level Items of the crate
// NOTE when extract_modules is called at the top level (eg crate items) we obviously want it to return Vec<ItemActual> since Vec<ItemRef> would not make sense as the refs wouldn't have anything to point to. However, when extract_modules is called for a submodule, we need to Vec<ItemRef> so the items can be stored in the RustMod, and for the actual items to be pushed directly to the top level Vec, to ensure we can record the correct index.
pub fn make_item_defs(
    items: Vec<Item>,
    // Same as `global_data.crate_path`, used for prepending module filepaths, except we don't have a `GlobalData` yet so we pass it in directly
    // None if we are extracting data for a single file or snippet, rather than an actual crate (so no `mod foo` allowed)
    // TODO crate_path might use hiphens instead of underscore as a word seperator, so need to ensure it is only used for file paths, and not module paths
    crate_path: &Option<PathBuf>,
    current_path: &mut Vec<String>,
    actual_item_defs: &mut Vec<ItemDefNoTypes>,
    // modules: &mut Vec<ModuleDataFirstPass>,
) -> Vec<ItemRef> {
    // let mut module_path_with_crate = vec!["crate".to_string()];
    // module_path_with_crate.extend(module_path.clone());
    // let current_module_data = modules
    //     .iter_mut()
    //     .find(|module| module.path == *module_path)
    //     .unwrap();
    // let defined_names = &mut current_module_data.defined_names;

    // dbg!(&module_path);

    let mut item_refs = Vec::new();
    // TODO the code for eg module.item_definitions.push(...) is a duplicated also for scope.item_definitons.push(...). Remove this duplication.
    for item in items {
        item_refs.push(item_to_item_ref(
            item.clone(),
            actual_item_defs,
            crate_path,
            current_path,
        ));
    }

    // if crate_path.is_some() {
    //     vec![ItemRef::Mod(RustMod {
    //         pub_: false,
    //         module_path: current_path.clone(),
    //         items: module_itemrefs,
    //     })]
    // } else {
    //     module_itemrefs
    // }
    item_refs
}

fn item_to_item_ref(
    item: Item,
    actual_item_defs: &mut Vec<ItemDefNoTypes>,
    crate_path: &Option<PathBuf>,
    current_path: &mut Vec<String>,
) -> ItemRef {
    match item {
        Item::Const(item_const) => {
            let const_name = item_const.ident.to_string();

            let is_pub = match item_const.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };

            let const_def = ConstDefNoTypes {
                ident: item_const.ident.clone(),
                is_pub,
                syn_object: item_const.clone(),
                expr: expr_to_expr_ref(
                    *item_const.expr,
                    actual_item_defs,
                    crate_path,
                    current_path,
                ),
            };
            actual_item_defs.push(ItemDefNoTypes::Const(const_def));
            ItemRef::Const(actual_item_defs.len() - 1)
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

            let is_copy = item_enum.attrs.iter().any(|attr| match &attr.meta {
                Meta::Path(_) => todo!(),
                Meta::List(meta_list) => {
                    let segs = &meta_list.path.segments;
                    if segs.len() == 1 && segs.first().unwrap().ident == "derive" {
                        let tokens = format!("({},)", meta_list.tokens);
                        let trait_tuple = syn::parse_str::<syn::TypeTuple>(&tokens).unwrap();
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
            });

            let is_pub = match item_enum.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };
            actual_item_defs.push(ItemDefNoTypes::StructEnum(StructEnumDefNoTypes {
                ident: item_enum.ident.clone(),
                is_copy,
                is_pub,
                generics,
                syn_generics: item_enum.generics.clone(),
                struct_or_enum_info: StructOrEnumDefitionInfo::Enum(item_enum.clone()),
                // impl_block_ids: Vec::new(),
            }));
            ItemRef::StructOrEnum(actual_item_defs.len() - 1)
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

            let is_pub = match item_fn.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };

            let rust_stmts = item_fn
                .block
                .stmts
                .clone()
                .into_iter()
                .map(|stmt| stmt_to_stmts_ref(stmt, actual_item_defs, crate_path, current_path))
                .collect();

            actual_item_defs.push(ItemDefNoTypes::Fn(FnDefNoTypes {
                attributes: item_fn.attrs.clone(),
                ident: item_fn.sig.ident.clone(),
                is_pub,
                generics,
                syn_generics: item_fn.sig.generics.clone(),
                signature: item_fn.sig.clone(),
                // syn: FnInfoSyn::Standalone(item_fn.clone()),
                stmts: rust_stmts,
                syn: FnInfoSyn::Standalone(item_fn.clone()),
            }));
            ItemRef::Fn(actual_item_defs.len() - 1)
        }
        Item::ForeignMod(_) => todo!(),
        Item::Impl(item_impl) => {
            // Extract modules from impl blocks

            // let mut rust_impl_items = Vec::new();
            // for item in &item_impl.items {
            //     match item {
            //         ImplItem::Const(_) => todo!(),
            //         ImplItem::Fn(impl_item_fn) => {
            //             // TODO dedupe with Item::Fn
            //             let generics = impl_item_fn
            //                 .sig
            //                 .generics
            //                 .params
            //                 .iter()
            //                 .filter_map(|g| match g {
            //                     GenericParam::Lifetime(_) => None,
            //                     GenericParam::Type(type_param) => {
            //                         Some(type_param.ident.to_string())
            //                     }
            //                     GenericParam::Const(_) => todo!(),
            //                 })
            //                 .collect::<Vec<_>>();

            //             let is_pub = match impl_item_fn.vis {
            //                 Visibility::Public(_) => true,
            //                 Visibility::Restricted(_) => todo!(),
            //                 Visibility::Inherited => false,
            //             };

            //             let stmts = impl_item_fn
            //                 .block
            //                 .stmts
            //                 .clone()
            //                 .into_iter()
            //                 .map(|stmt| {
            //                     stmt_to_stmts_ref(stmt, actual_item_defs, crate_path, current_path)
            //                 })
            //                 .collect();

            //             rust_impl_items.push(ImplItemV1::Fn(FnInfo {
            //                 ident: impl_item_fn.sig.ident.to_string(),
            //                 is_pub,
            //                 generics,
            //                 signature: impl_item_fn.sig.clone(),
            //                 stmts,
            //                 syn: FnInfoSyn::Impl(impl_item_fn.clone()),
            //             }))
            //         }
            //         ImplItem::Type(_) => todo!(),
            //         ImplItem::Macro(_) => todo!(),
            //         ImplItem::Verbatim(_) => todo!(),
            //         _ => todo!(),
            //     }
            // }

            let item_actual = ItemDefNoTypes::Impl(
                item_impl.clone(),
                item_impl
                    .items
                    .into_iter()
                    .map(|impl_item| match impl_item {
                        ImplItem::Const(_) => ImplItemExprStmtRefs::Const,
                        ImplItem::Fn(impl_item_fn) => ImplItemExprStmtRefs::Fn(
                            impl_item_fn
                                .block
                                .stmts
                                .into_iter()
                                .map(|stmt| {
                                    stmt_to_stmts_ref(
                                        stmt,
                                        actual_item_defs,
                                        crate_path,
                                        current_path,
                                    )
                                })
                                .collect(),
                        ),
                        ImplItem::Type(_) => todo!(),
                        ImplItem::Macro(_) => todo!(),
                        ImplItem::Verbatim(_) => todo!(),
                        _ => todo!(),
                    })
                    .collect(),
            );
            actual_item_defs.push(item_actual);
            ItemRef::Impl(actual_item_defs.len() - 1)
        }
        Item::Macro(_) => {
            //
            ItemRef::Macro
        }
        // We have already split up the modules in individual `ModuleData`s (which we are currently iterating through) so should ignore `Item::Mod`s
        Item::Mod(item_mod) => {
            let pub_ = match item_mod.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };

            // let module_data = modules.get_mut(current_path);
            // match item_mod.vis {
            //     Visibility::Public(_) => {
            //         module_data.pub_submodules.push(item_mod.ident.to_string())
            //     }
            //     Visibility::Restricted(_) => todo!(),
            //     Visibility::Inherited => module_data
            //         .private_submodules
            //         .push(item_mod.ident.to_string()),
            // }

            let _parent_name = current_path.last().cloned();
            current_path.push(item_mod.ident.to_string());

            let mut rust_mod = RustMod {
                pub_,
                module_path: current_path.clone(),
                items: Vec::new(),
            };

            // let mut partial_module_data = ModuleDataFirstPass::new(
            //     item_mod.ident.to_string(),
            //     // parent_name,
            //     current_path.clone(),
            // );

            // NOTE we do the `modules.push(ModuleData { ...` below because we need to get the module items from the different content/no content branches
            if let Some(content) = item_mod.content {
                // partial_module_data.items.clone_from(&content.1);
                // modules.push(partial_module_data);

                // TODO how does `mod bar { mod foo; }` work?
                rust_mod.items =
                    make_item_defs(content.1, crate_path, current_path, actual_item_defs);
            } else if let Some(crate_path2) = crate_path {
                let mut file_path = crate_path2.clone();
                file_path.push("src");
                // IMPORTANT TODO need to check for "crate" *and* "my_external_crate", and also use the corrent `crate_path`
                if *current_path == ["crate"] {
                    file_path.push("main.rs");
                } else {
                    let mut module_path_copy = current_path.clone();
                    // remove "crate"
                    module_path_copy.remove(0);
                    let last = module_path_copy.last_mut().unwrap();
                    last.push_str(".rs");
                    file_path.extend(module_path_copy);
                }

                let code = fs::read_to_string(&file_path).unwrap();
                let file = syn::parse_file(&code).unwrap();

                // partial_module_data.items.clone_from(&file.items);
                // modules.push(partial_module_data);
                rust_mod.items =
                    make_item_defs(file.items, crate_path, current_path, actual_item_defs);
            } else {
                panic!("`mod foo` is not allowed in files/modules/snippets, only crates")
            }
            current_path.pop();

            ItemRef::Mod(rust_mod)
        }
        Item::Static(_) => todo!(),
        Item::Struct(item_struct) => {
            let generics = item_struct
                .generics
                .params
                .iter()
                .filter_map(|p| match p {
                    GenericParam::Lifetime(_) => None,
                    GenericParam::Type(type_param) => Some(type_param.ident.to_string()),
                    GenericParam::Const(_) => todo!(),
                })
                .collect::<Vec<_>>();

            let is_copy = item_struct.attrs.iter().any(|attr| match &attr.meta {
                Meta::Path(_) => todo!(),
                Meta::List(meta_list) => {
                    let segs = &meta_list.path.segments;
                    if segs.len() == 1 && segs.first().unwrap().ident == "derive" {
                        let tokens = format!("({},)", meta_list.tokens);
                        // NOTE can't parse as syn::TypeTuple because eg (Default) is not a tuple, only len > 1 like (Default, Debug)
                        let trait_tuple = syn::parse_str::<syn::TypeTuple>(&tokens).unwrap();
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
            });

            let is_pub = match item_struct.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };
            actual_item_defs.push(ItemDefNoTypes::StructEnum(StructEnumDefNoTypes {
                ident: item_struct.ident.clone(),
                is_pub,
                is_copy,
                generics,
                syn_generics: item_struct.generics.clone(),
                struct_or_enum_info: StructOrEnumDefitionInfo::Struct(item_struct.clone()),
                // impl_block_ids: Vec::new(),
            }));
            ItemRef::StructOrEnum(actual_item_defs.len() - 1)
        }
        Item::Trait(item_trait) => {
            let is_pub = match item_trait.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };
            let default_impls = item_trait
                .items
                .iter()
                .filter_map(|item| {
                    match item {
                        TraitItem::Const(_) => todo!(),
                        TraitItem::Fn(item_fn) => {
                            if let Some(block) = &item_fn.default {
                                let generics = item_fn
                                    .sig
                                    .generics
                                    .params
                                    .iter()
                                    .filter_map(|g| match g {
                                        GenericParam::Lifetime(_) => None,
                                        GenericParam::Type(type_param) => {
                                            Some(type_param.ident.to_string())
                                        }
                                        GenericParam::Const(_) => todo!(),
                                    })
                                    .collect::<Vec<_>>();

                                let rust_stmts = block
                                    .stmts
                                    .clone()
                                    .into_iter()
                                    .map(|stmt| {
                                        stmt_to_stmts_ref(
                                            stmt,
                                            actual_item_defs,
                                            crate_path,
                                            current_path,
                                        )
                                    })
                                    .collect();

                                Some(FnDefNoTypes {
                                    attributes: item_fn.attrs.clone(),
                                    ident: item_fn.sig.ident.clone(),
                                    is_pub,
                                    generics,
                                    syn_generics: item_fn.sig.generics.clone(),
                                    signature: item_fn.sig.clone(),
                                    // syn: FnInfoSyn::Standalone(item_fn.clone()),
                                    stmts: rust_stmts,
                                    syn: FnInfoSyn::Trait(item_fn.clone()),
                                })
                            } else {
                                None
                            }
                        }
                        TraitItem::Type(_) => todo!(),
                        TraitItem::Macro(_) => todo!(),
                        TraitItem::Verbatim(_) => todo!(),
                        _ => todo!(),
                    }
                })
                .collect();
            actual_item_defs.push(ItemDefNoTypes::Trait(TraitDefNoTypes {
                ident: item_trait.ident.clone(),
                is_pub,
                syn: item_trait.clone(),
                default_impls,
            }));
            ItemRef::Trait(actual_item_defs.len() - 1)
        }
        Item::TraitAlias(_) => todo!(),
        Item::Type(_) => todo!(),
        Item::Union(_) => todo!(),
        Item::Use(item_use) => {
            // TODO we are adding all use stmts the the module use mappings rather than accounting for when we are not at the top level so the stmts should be added to the scope? Also does `resolve_path()` account for the difference?
            // let module = modules.get_mut(current_path);

            ItemRef::Use(handle_item_use2(&item_use))
        }
        Item::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}

pub fn stmt_to_stmts_ref(
    stmt: Stmt,
    actual_item_defs: &mut Vec<ItemDefNoTypes>,
    crate_path: &Option<PathBuf>,
    current_path: &mut Vec<String>,
) -> StmtsRef {
    match stmt {
        Stmt::Local(local) => StmtsRef::Local(LocalRef {
            attrs: local.attrs,
            pat: local.pat,
            init: local.init.map(|local_init| LocalInitRef {
                expr: Box::new(expr_to_expr_ref(
                    *local_init.expr,
                    actual_item_defs,
                    crate_path,
                    current_path,
                )),
                diverge: local_init.diverge.map(|(_, diverge)| {
                    Box::new(expr_to_expr_ref(
                        *diverge,
                        actual_item_defs,
                        crate_path,
                        current_path,
                    ))
                }),
            }),
        }),
        Stmt::Item(item) => StmtsRef::Item(item_to_item_ref(
            item,
            actual_item_defs,
            crate_path,
            current_path,
        )),
        Stmt::Expr(expr, semi) => StmtsRef::Expr(
            expr_to_expr_ref(expr, actual_item_defs, crate_path, current_path),
            semi.is_some(),
        ),
        Stmt::Macro(stmt_macro) => StmtsRef::Macro(RustExprOrStmtMacro {
            attrs: stmt_macro.attrs,
            mac: stmt_macro.mac,
            semi_token: stmt_macro.semi_token.is_some(),
        }),
    }
}

pub fn expr_to_expr_ref(
    expr: Expr,
    actual_item_defs: &mut Vec<ItemDefNoTypes>,
    crate_path: &Option<PathBuf>,
    current_path: &mut Vec<String>,
) -> ExprRef {
    match expr {
        Expr::Array(expr_array) => ExprRef::Array(RustExprArray {
            attrs: expr_array.attrs,
            elems: expr_array
                .elems
                .into_iter()
                .map(|expr| expr_to_expr_ref(expr, actual_item_defs, crate_path, current_path))
                .collect(),
        }),
        Expr::Assign(expr_assign) => ExprRef::Assign(RustExprAssign {
            attrs: expr_assign.attrs,
            left: Box::new(expr_to_expr_ref(
                *expr_assign.left,
                actual_item_defs,
                crate_path,
                current_path,
            )),
            right: Box::new(expr_to_expr_ref(
                *expr_assign.right,
                actual_item_defs,
                crate_path,
                current_path,
            )),
        }),
        Expr::Async(expr_async) => ExprRef::Async(RustExprAsync {
            attrs: expr_async.attrs,
            capture: expr_async.capture.is_some(),
            stmts: expr_async
                .block
                .stmts
                .into_iter()
                .map(|stmt| stmt_to_stmts_ref(stmt, actual_item_defs, crate_path, current_path))
                .collect(),
        }),
        Expr::Await(expr_await) => ExprRef::Await(RustExprAwait {
            attrs: expr_await.attrs,
            base: Box::new(expr_to_expr_ref(
                *expr_await.base,
                actual_item_defs,
                crate_path,
                current_path,
            )),
        }),
        Expr::Binary(expr_binary) => ExprRef::Binary(RustExprBinary {
            attrs: expr_binary.attrs,
            left: Box::new(expr_to_expr_ref(
                *expr_binary.left,
                actual_item_defs,
                crate_path,
                current_path,
            )),
            op: expr_binary.op,
            right: Box::new(expr_to_expr_ref(
                *expr_binary.right,
                actual_item_defs,
                crate_path,
                current_path,
            )),
        }),
        Expr::Block(expr_block) => ExprRef::Block(RustExprBlock {
            attrs: expr_block.attrs,
            label: expr_block.label,
            stmts: expr_block
                .block
                .stmts
                .into_iter()
                .map(|stmt| stmt_to_stmts_ref(stmt, actual_item_defs, crate_path, current_path))
                .collect(),
        }),
        Expr::Break(_) => todo!(),
        Expr::Call(expr_call) => ExprRef::Call(RustExprCall {
            attrs: expr_call.attrs,
            func: Box::new(expr_to_expr_ref(
                *expr_call.func,
                actual_item_defs,
                crate_path,
                current_path,
            )),
            args: expr_call
                .args
                .into_iter()
                .map(|expr| expr_to_expr_ref(expr, actual_item_defs, crate_path, current_path))
                .collect(),
        }),
        Expr::Cast(_) => todo!(),
        Expr::Closure(expr_closure) => ExprRef::Closure(RustExprClosure {
            attrs: expr_closure.attrs,
            lifetimes: expr_closure.lifetimes,
            constness: expr_closure.constness.is_some(),
            movability: expr_closure.movability.is_some(),
            asyncness: expr_closure.asyncness.is_some(),
            capture: expr_closure.capture.is_some(),
            inputs: expr_closure.inputs.into_iter().collect(),
            output: expr_closure.output,
            body: Box::new(expr_to_expr_ref(
                *expr_closure.body,
                actual_item_defs,
                crate_path,
                current_path,
            )),
        }),
        Expr::Const(_) => todo!(),
        Expr::Continue(_) => todo!(),
        Expr::Field(expr_field) => ExprRef::Field(RustExprField {
            attrs: expr_field.attrs,
            base: Box::new(expr_to_expr_ref(
                *expr_field.base,
                actual_item_defs,
                crate_path,
                current_path,
            )),
            member: expr_field.member,
        }),
        Expr::ForLoop(expr_for_loop) => ExprRef::ForLoop(RustExprForLoop {
            attrs: expr_for_loop.attrs,
            label: expr_for_loop.label,
            pat: expr_for_loop.pat,
            expr: Box::new(expr_to_expr_ref(
                *expr_for_loop.expr,
                actual_item_defs,
                crate_path,
                current_path,
            )),
            body: expr_for_loop
                .body
                .stmts
                .into_iter()
                .map(|stmt| stmt_to_stmts_ref(stmt, actual_item_defs, crate_path, current_path))
                .collect(),
        }),
        Expr::Group(_) => todo!(),
        Expr::If(expr_if) => ExprRef::If(RustExprIf {
            attrs: expr_if.attrs,
            cond: Box::new(expr_to_expr_ref(
                *expr_if.cond,
                actual_item_defs,
                crate_path,
                current_path,
            )),
            then_branch: expr_if
                .then_branch
                .stmts
                .into_iter()
                .map(|stmt| stmt_to_stmts_ref(stmt, actual_item_defs, crate_path, current_path))
                .collect(),
            else_branch: expr_if.else_branch.map(|(_, expr)| {
                Box::new(expr_to_expr_ref(
                    *expr,
                    actual_item_defs,
                    crate_path,
                    current_path,
                ))
            }),
        }),
        Expr::Index(expr_index) => ExprRef::Index(RustExprIndex {
            attrs: expr_index.attrs,
            expr: Box::new(expr_to_expr_ref(
                *expr_index.expr,
                actual_item_defs,
                crate_path,
                current_path,
            )),
            index: Box::new(expr_to_expr_ref(
                *expr_index.index,
                actual_item_defs,
                crate_path,
                current_path,
            )),
        }),
        Expr::Infer(_) => todo!(),
        Expr::Let(expr_let) => ExprRef::Let(RustExprLet {
            attrs: expr_let.attrs,
            pat: expr_let.pat,
            expr: Box::new(expr_to_expr_ref(
                *expr_let.expr,
                actual_item_defs,
                crate_path,
                current_path,
            )),
        }),
        Expr::Lit(expr_lit) => ExprRef::Lit(RustExprLit {
            attrs: expr_lit.attrs,
            lit: expr_lit.lit,
        }),
        Expr::Loop(_) => todo!(),
        Expr::Macro(expr_macro) => ExprRef::Macro(RustExprOrStmtMacro {
            attrs: expr_macro.attrs,
            mac: expr_macro.mac,
            semi_token: false,
        }),
        Expr::Match(expr_match) => ExprRef::Match(RustExprMatch {
            attrs: expr_match.attrs,
            expr: Box::new(expr_to_expr_ref(
                *expr_match.expr,
                actual_item_defs,
                crate_path,
                current_path,
            )),
            arms: expr_match
                .arms
                .into_iter()
                .map(|arm| RustArm {
                    attrs: arm.attrs,
                    pat: arm.pat,
                    guard: arm.guard.map(|(_, expr)| {
                        Box::new(expr_to_expr_ref(
                            *expr,
                            actual_item_defs,
                            crate_path,
                            current_path,
                        ))
                    }),
                    body: Box::new(expr_to_expr_ref(
                        *arm.body,
                        actual_item_defs,
                        crate_path,
                        current_path,
                    )),
                    comma: arm.comma.is_some(),
                })
                .collect(),
        }),
        Expr::MethodCall(expr_method_call) => ExprRef::MethodCall(RustExprMethodCall {
            attrs: expr_method_call.attrs,
            receiver: Box::new(expr_to_expr_ref(
                *expr_method_call.receiver,
                actual_item_defs,
                crate_path,
                current_path,
            )),
            method: expr_method_call.method,
            turbofish: expr_method_call.turbofish,
            args: expr_method_call
                .args
                .into_iter()
                .map(|expr| expr_to_expr_ref(expr, actual_item_defs, crate_path, current_path))
                .collect(),
        }),
        Expr::Paren(_) => todo!(),
        Expr::Path(expr_path) => ExprRef::Path(RustExprPath {
            attrs: expr_path.attrs,
            qself: expr_path.qself,
            path: expr_path.path,
        }),
        Expr::Range(_) => todo!(),
        Expr::Reference(expr_reference) => ExprRef::Reference(RustExprReference {
            attrs: expr_reference.attrs,
            mutability: expr_reference.mutability.is_some(),
            expr: Box::new(expr_to_expr_ref(
                *expr_reference.expr,
                actual_item_defs,
                crate_path,
                current_path,
            )),
        }),
        Expr::Repeat(_) => todo!(),
        Expr::Return(expr_return) => ExprRef::Return(RustExprReturn {
            attrs: expr_return.attrs,
            expr: expr_return.expr.map(|expr| {
                Box::new(expr_to_expr_ref(
                    *expr,
                    actual_item_defs,
                    crate_path,
                    current_path,
                ))
            }),
        }),
        Expr::Struct(expr_struct) => ExprRef::Struct(RustExprStruct {
            attrs: expr_struct.attrs,
            qself: expr_struct.qself,
            path: expr_struct.path,
            fields: expr_struct
                .fields
                .into_iter()
                .map(|field| RustFieldValue {
                    attrs: field.attrs,
                    member: field.member,
                    colon_token: field.colon_token.is_some(),
                    expr: expr_to_expr_ref(field.expr, actual_item_defs, crate_path, current_path),
                })
                .collect(),
            dot2_token: expr_struct.dot2_token.is_some(),
            rest: expr_struct.rest.map(|expr| {
                Box::new(expr_to_expr_ref(
                    *expr,
                    actual_item_defs,
                    crate_path,
                    current_path,
                ))
            }),
        }),
        Expr::Try(_) => todo!(),
        Expr::TryBlock(_) => todo!(),
        Expr::Tuple(expr_tuple) => ExprRef::Tuple(RustExprTuple {
            attrs: expr_tuple.attrs,
            elems: expr_tuple
                .elems
                .into_iter()
                .map(|expr| expr_to_expr_ref(expr, actual_item_defs, crate_path, current_path))
                .collect(),
        }),
        Expr::Unary(expr_unary) => ExprRef::Unary(RustExprUnary {
            attrs: expr_unary.attrs,
            op: expr_unary.op,
            expr: Box::new(expr_to_expr_ref(
                *expr_unary.expr,
                actual_item_defs,
                crate_path,
                current_path,
            )),
        }),
        Expr::Unsafe(expr_unsafe) => ExprRef::Unsafe(RustExprUnsafe {
            attrs: expr_unsafe.attrs,
            block: RustExprBlock {
                attrs: vec![],
                label: None,
                stmts: expr_unsafe
                    .block
                    .stmts
                    .into_iter()
                    .map(|stmt| stmt_to_stmts_ref(stmt, actual_item_defs, crate_path, current_path))
                    .collect(),
            },
        }),
        Expr::Verbatim(_) => todo!(),
        Expr::While(_) => todo!(),
        Expr::Yield(_) => todo!(),
        _ => todo!(),
    }
}

/// We want each of used items to return the name of the item, and the path relative to the root module, eg:
/// eg `use mod::sub_mod::{item1, item2, another_mod::item3}` -> [mod/sub_mod/item1, mod/sub_mod/item2, mod/sub_mod/another_mod/item3]
///
/// `relative_path` (snake) is a temporary var for building the relative path that gets copied into `items`
///
/// items is what gets stored in global_data  Vec<(item name (snake), relative path (snake))>
///
pub fn tree_parsing_for_boilerplate(
    use_tree: &UseTree,
    // We push to `relative_path` to build up a path for each of the items/modules "imported"/`use`d by the use stmt
    relative_path: &mut Vec<String>,
    items: &mut Vec<(String, Vec<String>)>,
) {
    match use_tree {
        UseTree::Path(use_path) => {
            relative_path.push(use_path.ident.to_string());
            tree_parsing_for_boilerplate(&use_path.tree, relative_path, items);
        }
        // NOTE a `syn::UseName` can the the ident for an item *or* a submodule that is being `use`d??
        UseTree::Name(use_name) => items.push((use_name.ident.to_string(), relative_path.clone())),
        UseTree::Rename(_) => todo!(),
        UseTree::Glob(_) => {
            println!("here");
            dbg!(relative_path);
            dbg!(items);
            println!("{}", quote! { #use_tree });
            todo!()
        }
        UseTree::Group(use_group) => {
            for item in &use_group.items {
                match item {
                    UseTree::Path(use_path) => {
                        // Create separate `relative_path`s for each "fork" created by the `UseGroup`
                        let mut new_relative_path = relative_path.clone();
                        new_relative_path.push(use_path.ident.to_string());
                        tree_parsing_for_boilerplate(&use_path.tree, &mut new_relative_path, items);
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

/// Populates module pub/private and scoped `.use_mappings`s
pub fn handle_item_use2(item_use: &ItemUse) -> RustUse {
    let pub_ = matches!(item_use.vis, Visibility::Public(_));

    let item_paths = match &item_use.tree {
        UseTree::Path(use_path) => {
            let _root_module_or_crate = use_path.ident.to_string();

            let mut item_paths = Vec::new();
            let mut relative_path = vec![use_path.ident.to_string()];
            tree_parsing_for_boilerplate(&use_path.tree, &mut relative_path, &mut item_paths);
            item_paths
        }
        // TODO need to consider what a simple `use foo` means, since for modules this would be preceeded by `mod foo` which has the same effect?
        UseTree::Name(_use_name) => todo!(),
        _ => panic!("root of use trees are always a path or name"),
    };

    RustUse {
        pub_,
        use_mappings: item_paths,
    }
}

#[derive(Debug, Clone)]
pub enum ImplItemExprStmtRefs {
    Fn(Vec<StmtsRef>),
    Const,
}

// Actual definitions (only use at top level)
#[derive(Debug, Clone)]
pub enum ItemDefNoTypes {
    StructEnum(StructEnumDefNoTypes),
    Fn(FnDefNoTypes),
    Const(ConstDefNoTypes),
    Trait(TraitDefNoTypes),
    // TODO replace these with proper type to be more consistent with othe variants?
    Impl(ItemImpl, Vec<ImplItemExprStmtRefs>),
    // Should never be handled, only used for empty initialisation
    // TODO replace use with Option?
    None,
    // Mod(RustMod),
    // Impl(RustMod),
    // Use(RustUse),
}
impl ItemDefNoTypes {
    pub fn ident(&self) -> &syn::Ident {
        match self {
            ItemDefNoTypes::StructEnum(def) => &def.ident,
            ItemDefNoTypes::Fn(def) => &def.ident,
            ItemDefNoTypes::Const(def) => &def.ident,
            ItemDefNoTypes::Trait(def) => &def.ident,
            // ItemActual::Impl(_) => panic!(),
            ItemDefNoTypes::Impl(_, _) => panic!(),
            ItemDefNoTypes::None => panic!(),
        }
    }
}

// References to top level deifinitions (used inside Mod's, Fn bodies)
// TODO make this Copy
#[derive(Debug, Clone)]
pub enum ItemRef {
    StructOrEnum(usize),
    Fn(usize),
    Const(usize),
    Trait(usize),
    Impl(usize),
    Mod(RustMod),
    Use(RustUse),
    Macro,
}
impl ItemRef {
    pub fn index(&self) -> Option<usize> {
        match self {
            ItemRef::StructOrEnum(index) => Some(*index),
            ItemRef::Fn(index) => Some(*index),
            ItemRef::Const(index) => Some(*index),
            ItemRef::Trait(index) => Some(*index),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct RustUse {
    pub pub_: bool,
    // pub item_name: String,
    // pub usepath: Vec<String>,
    pub use_mappings: Vec<(String, Vec<String>)>,
}

#[derive(Debug, Clone)]
pub struct RustMod {
    pub pub_: bool,
    pub module_path: Vec<String>,
    // pub pub_use_mappings: Vec<(String, Vec<String>)>,
    // pub private_use_mappings: Vec<(String, Vec<String>)>,
    pub items: Vec<ItemRef>,
}
impl RustMod {
    pub fn item_defined_in_module(
        &self,
        items: &[ItemDefNoTypes],
        use_private: bool,
        name: &str,
    ) -> Option<usize> {
        self.items.iter().find_map(|item| match item {
            ItemRef::StructOrEnum(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemDefNoTypes::StructEnum(def) => def,
                    _ => todo!(),
                };
                (def.ident == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Fn(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemDefNoTypes::Fn(fn_info) => fn_info,
                    _ => todo!(),
                };
                (def.ident == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Const(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemDefNoTypes::Const(def) => def,
                    _ => todo!(),
                };
                (def.ident == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Trait(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemDefNoTypes::Trait(def) => def,
                    _ => todo!(),
                };
                (def.ident == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Mod(_) => None,
            ItemRef::Impl(_) => None,
            ItemRef::Use(_) => None,
            ItemRef::Macro => None,
        })
    }
    pub fn path_starts_with_sub_module(&self, use_private: bool, ident: &str) -> bool {
        self.items.iter().any(|item| match item {
            ItemRef::Mod(rust_mod) => {
                rust_mod.module_path[rust_mod.module_path.len() - 1] == ident
                    && (use_private || rust_mod.pub_)
            }
            _ => false,
        })
    }

    pub fn item_defined_in_module2(
        &self,
        items: &[ItemDefRc],
        use_private: bool,
        name: &str,
    ) -> Option<usize> {
        self.items.iter().find_map(|item| match item {
            ItemRef::StructOrEnum(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemDefRc::StructEnum(def) => def,
                    _ => todo!(),
                };
                (def.ident == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Fn(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemDefRc::Fn(fn_info) => fn_info,
                    _ => todo!(),
                };
                (def.sig.ident == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Const(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemDefRc::Const(def) => def,
                    _ => todo!(),
                };
                (def.ident == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Trait(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemDefRc::Trait(def) => def,
                    _ => todo!(),
                };
                (def.ident == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Mod(_) => None,
            ItemRef::Impl(_) => None,
            ItemRef::Use(_) => None,
            ItemRef::Macro => todo!(),
        })
    }
}

#[derive(Debug, Clone)]
pub enum StmtsRef {
    Local(LocalRef),
    Item(ItemRef),
    Expr(ExprRef, bool),
    Macro(RustExprOrStmtMacro),
}
#[derive(Debug, Clone)]
pub struct RustExprOrStmtMacro {
    pub attrs: Vec<Attribute>,
    pub mac: Macro,
    pub semi_token: bool,
}
#[derive(Debug, Clone)]
pub struct LocalRef {
    pub attrs: Vec<Attribute>,
    pub pat: Pat,
    pub init: Option<LocalInitRef>,
}
#[derive(Debug, Clone)]
pub struct LocalInitRef {
    pub expr: Box<ExprRef>,
    pub diverge: Option<Box<ExprRef>>,
}

#[derive(Debug, Clone)]
pub enum StructOrEnumDefitionInfo {
    Struct(ItemStruct),
    Enum(ItemEnum),
}

/// Similar to StructOrEnum which gets used in RustType, but is for storing info about the actual item definition, rather than instances of, so eg we don't need to be able to store resolved generics. Minor differences but making distinct type helps with reasoning about the different use cases.
/// Just structs and enums or should we include functions?
#[derive(Debug, Clone)]
pub struct StructEnumDefNoTypes {
    pub ident: Ident,
    // NOTE we don't need to store the module path because module level `ItemDefinition`s are stored within modules so we will already know the module path
    // module_path: Option<Vec<String>>,
    pub is_copy: bool,
    pub is_pub: bool,
    // TODO do we need to know eg bounds for each generic? Yes, but how do we store the trait considering even it might not have been parsed yet so won't have an index?
    // pub generics: Vec<RustTypeParam>,
    pub generics: Vec<String>,
    pub syn_generics: Generics,
    // syn_object: StructOrEnumSynObject,
    pub struct_or_enum_info: StructOrEnumDefitionInfo,
    // pub impl_block_ids: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct TraitDefNoTypes {
    pub ident: Ident,
    pub is_pub: bool,
    pub syn: ItemTrait,
    pub default_impls: Vec<FnDefNoTypes>,
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

#[derive(Debug, Clone)]
pub struct ConstDefNoTypes {
    pub ident: Ident,
    pub is_pub: bool,
    pub syn_object: ItemConst,
    pub expr: ExprRef,
}

/// Not just for methods, can also be an enum variant with no inputs
#[derive(Debug, Clone)]
pub struct FnDefNoTypes {
    pub attributes: Vec<Attribute>,
    pub ident: Ident,
    pub is_pub: bool,
    // NOTE see StructEnumDef
    // pub generics: Vec<RustTypeParam>,
    pub generics: Vec<String>,
    pub syn_generics: Generics,
    // TODO remove this, just legacy thing we need for now because it gets used in the JS parsing (I think)
    pub syn: FnInfoSyn,

    pub signature: Signature,
    // pub syn: FnInfoSyn,
    pub stmts: Vec<StmtsRef>,
}

#[derive(Debug, Clone)]
pub enum FnInfoSyn {
    Standalone(ItemFn),
    Impl(ImplItemFn),
    #[allow(dead_code)]
    Trait(TraitItemFn),
}

#[allow(dead_code)]
pub trait ModuleMethods {
    fn lookup_trait_definition_any_module<I>(
        &self,
        current_module_path: &[String],
        current_scope_id: &Option<Vec<usize>>,
        // path: &Vec<String>,
        path: I,
        // current_module: &Vec<String>,
    ) -> (Vec<String>, Option<Vec<usize>>, TraitDefNoTypes)
    where
        // I: IntoIterator<Item = String>,
        I: IntoIterator,
        I::Item: AsRef<str>;
}

#[allow(clippy::too_many_arguments)]
// NOTE rather than thinking about this version of resolve_path as simply being for the make_item_defs stage, we should simply think about it as the version to use when we know we are looking for an item, not a var. What about partial items? If not then we can return the  actual item def instead of the index? No because we use it in update_item_def where the updated item def might not exist yet which is why we just want the index. If we got rid of the StructEnum module_path etc in RustType then we could at least probably get rid of the other return fields and just return the index?
pub fn resolve_path(
    // look_for_scoped_vars: bool,
    // TODO can we combine this with `look_for_scoped_vars`?
    _look_for_scoped_items: bool,
    use_private_items: bool,
    mut segs: Vec<RustPathSegment>,
    crates: &[RustMod],
    // TODO replace GlobalData with `.modules` and `.scopes` to making setting up test cases easier
    // global_data: &GlobalData,
    items_defs: &[ItemDefNoTypes],
    // scopes: &[GlobalDataScope],
    current_mod: &[String],
    // Only used to determine if current module is the original module
    orig_mod: &[String],
    scoped_items: &[Vec<ItemRef>],
    // TODO we are not handling vars in this version so index should not be in Option?
) -> (Vec<String>, Vec<RustPathSegment>, bool, Option<usize>) {
    debug!(segs = ?segs, "get_path_without_namespacing");

    // TODO I don't think we need to pass in the module `ModuleData` if we are already passing the `current_module` module path we can just use that to look it up each time, which might be less efficient since we shouldn't need to lookup the module if we haven't changed modules (though I think we are pretty much always changing modules except for use statements?), but we definitely don't want to pass in both. Maybe only pass in `module: &ModuleData` and not `current_module`
    // assert!(current_module == &module.path);

    // dbg!(&item_refs);
    // dbg!(&current_mod);
    // dbg!(crates
    //     .iter()
    //     .map(|rust_mod| &rust_mod.module_path)
    //     .collect::<Vec<_>>());
    // dbg!(&segs);
    let module = look_for_module_in_crates(crates, items_defs, current_mod).unwrap();

    // dbg!(&segs);
    // dbg!(&current_mod);
    // dbg!(&orig_mod);
    // dbg!(&current_scope_id);
    // dbg!(&module.scoped_various_definitions);

    let is_parent_or_same_module = if orig_mod.len() >= current_mod.len() {
        current_mod
            .iter()
            .enumerate()
            .all(|(i, current_module)| current_module == &orig_mod[i])
    } else {
        false
    };

    let use_private = use_private_items || is_parent_or_same_module;
    let item_defined_in_module =
        module.item_defined_in_module(items_defs, use_private, &segs[0].ident);

    let path_starts_with_sub_module = module.path_starts_with_sub_module(
        use_private_items || is_parent_or_same_module,
        &segs[0].ident,
    );

    // TODO only look through transparent scopes
    let matched_use_mapping = module.items.iter().find_map(|item| match item {
        // TODO make this recursive rather than only looking 1 fn deep and ensure any use stmts used anywhere are found
        ItemRef::Fn(index) => match &items_defs[*index] {
            ItemDefNoTypes::Fn(fn_def_no_types) => {
                fn_def_no_types
                    .stmts
                    .iter()
                    .find_map(|stmt_ref| match stmt_ref {
                        StmtsRef::Item(item_ref) => match item_ref {
                            ItemRef::Use(rust_use) => {
                                rust_use.use_mappings.iter().find_map(|use_mapping| {
                                    (use_mapping.0 == segs[0].ident
                                        && (use_private || rust_use.pub_))
                                        .then_some(use_mapping.clone())
                                })
                            }
                            _ => None,
                        },
                        _ => None,
                    })
            }
            _ => panic!(),
        },
        ItemRef::Use(rust_use) => rust_use.use_mappings.iter().find_map(|use_mapping| {
            (use_mapping.0 == segs[0].ident && (use_private || rust_use.pub_))
                .then_some(use_mapping.clone())
        }),
        _ => None,
    });

    // TODO can module shadow external crate names? In which case we need to look for modules first? I think we do this implicitly based on the order of the if statements below?
    // TODO actually look up external crates in Cargo.toml
    let external_crate_names = ["web_prelude", "std"];
    let path_is_external_crate = external_crate_names.iter().any(|cn| cn == &segs[0].ident);

    // Look for scoped item
    let scoped_item = scoped_items.iter().rev().find_map(|s| {
        s.iter().find_map(|item| match item {
            ItemRef::StructOrEnum(index) => {
                let item = &items_defs[*index];
                let def = match item {
                    ItemDefNoTypes::StructEnum(def) => def,
                    _ => todo!(),
                };
                (def.ident == segs[0].ident).then_some(*index)
            }
            ItemRef::Fn(index) => {
                let item = &items_defs[*index];
                let def = match item {
                    ItemDefNoTypes::Fn(fn_info) => fn_info,
                    _ => todo!(),
                };
                (def.ident == segs[0].ident).then_some(*index)
            }
            ItemRef::Const(index) => {
                let item = &items_defs[*index];
                let def = match item {
                    ItemDefNoTypes::Const(def) => def,
                    _ => todo!(),
                };
                (def.ident == segs[0].ident).then_some(*index)
            }
            ItemRef::Trait(index) => {
                let item = &items_defs[*index];
                let def = match item {
                    ItemDefNoTypes::Trait(def) => def,
                    _ => todo!(),
                };
                (def.ident == segs[0].ident).then_some(*index)
            }
            ItemRef::Mod(_) => None,
            ItemRef::Impl(_) => None,
            ItemRef::Use(_) => None,
            ItemRef::Macro => todo!(),
        })
    });

    // TODO not sure why we need use_private_items here
    // if use_private_items && is_scoped {
    // if is_scoped || is_scoped_static {
    // dbg!(&is_scoped_static_scope);
    if scoped_item.is_some() {
        // Variables and scoped items
        // Need to handle scoped vars and items first, otherwise when handling as module paths, we would always first have to check if the path is a scoped var/item

        // If we are returning a scoped var/item, no recursion should have occured so we should be in the same module
        assert!(current_mod == orig_mod);
        // (current_mod.clone(), segs, is_scoped_static)
        (
            current_mod.to_vec(),
            segs,
            scoped_item.is_some(),
            scoped_item,
        )
    } else if item_defined_in_module.is_some() {
        (current_mod.to_vec(), segs, false, item_defined_in_module)
    } else if segs[0].ident == "super" {
        // TODO if a module level item name is shadowed by an item in a fn scope, then module level item needs to be namespaced
        segs.remove(0);

        let mut current_module = current_mod.to_vec();
        current_module.pop();

        resolve_path(
            false,
            true,
            segs,
            crates,
            items_defs,
            &current_module,
            orig_mod,
            scoped_items,
        )
    } else if segs[0].ident == "self" {
        // NOTE private items are still accessible from the module via self
        segs.remove(0);

        resolve_path(
            false,
            true,
            segs,
            crates,
            items_defs,
            current_mod,
            orig_mod,
            scoped_items,
        )
    } else if segs[0].ident == "crate" {
        let current_module = vec!["crate".to_string()];

        segs.remove(0);

        resolve_path(
            false,
            true,
            segs,
            crates,
            items_defs,
            &current_module,
            orig_mod,
            scoped_items,
        )
    } else if path_starts_with_sub_module {
        // Path starts with a submodule of the current module
        let mut submod_path = current_mod.to_vec();
        submod_path.push(segs[0].ident.to_string());

        segs.remove(0);

        resolve_path(
            false,
            false,
            segs,
            crates,
            items_defs,
            &submod_path,
            orig_mod,
            scoped_items,
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

        resolve_path(
            false,
            true,
            use_segs,
            crates,
            items_defs,
            // &new_mod,
            current_mod,
            // &use_mapping.1.clone(),
            orig_mod,
            scoped_items,
        )
    // } else if segs.len() == 1 && segs[0] == "this" {
    //     segs
    } else if path_is_external_crate {
        // } else if false {
        // TODO need to update current_mod

        // Handle equivalently to segs[0] == "crate"
        let crate_name = segs.remove(0);
        let current_module = [crate_name.ident].to_vec();

        resolve_path(
            false,
            true,
            segs,
            crates,
            items_defs,
            &current_module,
            orig_mod,
            scoped_items,
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
        // dbg!(&segs);
        assert!(segs.len() == 1 || segs.len() == 2);
        let seg = &segs[0];
        if seg.ident == "i32"
            || seg.ident == "String"
            || seg.ident == "str"
            || seg.ident == "bool"
            || seg.ident == "Some"
            || seg.ident == "None"
            || (seg.ident == "Box" && &segs[1].ident == "new")
            || seg.ident == "FnOnce"
            || seg.ident == "Copy"
            || seg.ident == "Vec"
            || seg.ident == "Option"
            || seg.ident == "Result"
        {
            // TODO shouldn't need this
            fn prelude_item_def_name_to_js(item_def_name: &str) -> &str {
                match item_def_name {
                    "bool" => "Bool",
                    other => other,
                }
            }
            let seg_new = prelude_item_def_name_to_js(&seg.ident);
            // TODO IMPORTANT we aren't meant to be handling these in get_path, they should be handled in the item def passes, not the JS parsing. add a panic!() here. NO not true, we will have i32, String, etc in closure defs, type def for var assignments, etc.
            // TODO properly encode "prelude_special_case" in a type rather than a String
            let item_index = crates
                .iter()
                .find_map(|rust_mod| {
                    if rust_mod.module_path == [RUST_PRELUDE_MODULE_PATH] {
                        rust_mod.items.iter().find_map(|item_ref| match item_ref {
                            ItemRef::StructOrEnum(index) => {
                                let item = &items_defs[*index];
                                match item {
                                    ItemDefNoTypes::StructEnum(def) => {
                                        (def.ident == seg_new).then_some(*index)
                                    }
                                    _ => todo!(),
                                }
                            }
                            ItemRef::Fn(index) => {
                                let item = &items_defs[*index];
                                match item {
                                    ItemDefNoTypes::Fn(def) => {
                                        (def.ident == seg_new).then_some(*index)
                                    }
                                    _ => todo!(),
                                }
                            }
                            ItemRef::Const(index) => {
                                let item = &items_defs[*index];
                                match item {
                                    ItemDefNoTypes::Const(def) => {
                                        (def.ident == seg_new).then_some(*index)
                                    }
                                    _ => todo!(),
                                }
                            }
                            ItemRef::Trait(index) => {
                                let item = &items_defs[*index];
                                match item {
                                    ItemDefNoTypes::Trait(def) => {
                                        (def.ident == seg_new).then_some(*index)
                                    }
                                    _ => todo!(),
                                }
                            }
                            _ => None,
                        })
                    } else {
                        None
                    }
                })
                .unwrap();
            (
                vec![RUST_PRELUDE_MODULE_PATH.to_string()],
                segs,
                false,
                Some(item_index),
            )
        } else {
            dbg!("resolve_path couldn't find path");
            // dbg!(module);
            dbg!(current_mod);
            // dbg!(current_scope_id);
            dbg!(segs);
            panic!()
        }
    }
}

pub fn look_for_module_in_crates(
    crates: &[RustMod],
    item_defs: &[ItemDefNoTypes],
    module_path: &[String],
) -> Option<RustMod> {
    for rust_mod in crates {
        if rust_mod.module_path == module_path {
            return Some(rust_mod.clone());
        }
        for item in &rust_mod.items {
            match item {
                ItemRef::Fn(index) => {
                    let item = &item_defs[*index];
                    let fn_info = match item {
                        ItemDefNoTypes::Fn(fn_info) => fn_info,
                        _ => todo!(),
                    };

                    let fn_body_items = fn_info
                        .stmts
                        .clone()
                        .into_iter()
                        .filter_map(|stmt| {
                            match stmt {
                                StmtsRef::Item(item) => Some(item),
                                // TODO
                                // StmtsV1::Expr(_, _) => todo!(),
                                _ => None,
                            }
                        })
                        .collect::<Vec<_>>();

                    let found_rust_mod =
                        look_for_module_in_items(&fn_body_items, item_defs, module_path);
                    if found_rust_mod.is_some() {
                        return found_rust_mod;
                    }
                }
                ItemRef::Mod(rust_mod) => {
                    if rust_mod.module_path == module_path {
                        return Some(rust_mod.clone());
                    }
                    let found_rust_mod =
                        look_for_module_in_items(&rust_mod.items, item_defs, module_path);
                    if found_rust_mod.is_some() {
                        return found_rust_mod;
                    }
                }
                // TODO
                // ItemV1::Impl(_) => {}
                // ItemV1::Use(_) => {}
                _ => {}
            }
        }
    }
    None
}

pub fn look_for_module_in_items(
    items: &[ItemRef],
    item_defs: &[ItemDefNoTypes],
    module_path: &[String],
) -> Option<RustMod> {
    for item in items {
        match item {
            ItemRef::Fn(index) => {
                let item = &item_defs[*index];
                let fn_info = match item {
                    ItemDefNoTypes::Fn(fn_info) => fn_info,
                    _ => todo!(),
                };

                let fn_body_items = fn_info
                    .stmts
                    .clone()
                    .into_iter()
                    .filter_map(|stmt| {
                        match stmt {
                            StmtsRef::Item(item) => Some(item),
                            // TODO
                            // StmtsV1::Expr(_, _) => todo!(),
                            _ => None,
                        }
                    })
                    .collect::<Vec<_>>();

                let found_rust_mod =
                    look_for_module_in_items(&fn_body_items, item_defs, module_path);
                if found_rust_mod.is_some() {
                    return found_rust_mod;
                }
            }
            ItemRef::Mod(rust_mod) => {
                if rust_mod.module_path == module_path {
                    return Some(rust_mod.clone());
                }
                let found_rust_mod =
                    look_for_module_in_items(&rust_mod.items, item_defs, module_path);
                if found_rust_mod.is_some() {
                    return found_rust_mod;
                }
            }
            // TODO
            // ItemV1::Impl(_) => {}
            // ItemV1::Use(_) => {}
            _ => {}
        }
    }
    None
}

// TODO put in expr_ref module
#[derive(Debug, Clone)]
pub enum ExprRef {
    Array(RustExprArray),
    Assign(RustExprAssign),
    Async(RustExprAsync),
    Await(RustExprAwait),
    Binary(RustExprBinary),
    Block(RustExprBlock),
    Break(RustExprBreak),
    Call(RustExprCall),
    Cast(RustExprCast),
    Closure(RustExprClosure),
    Const(RustExprConst),
    Continue(RustExprContinue),
    Field(RustExprField),
    ForLoop(RustExprForLoop),
    Group(RustExprGroup),
    If(RustExprIf),
    Index(RustExprIndex),
    Infer(RustExprInfer),
    Let(RustExprLet),
    Lit(RustExprLit),
    Loop(RustExprLoop),
    // Macro(RustExprMacro),
    Macro(RustExprOrStmtMacro),
    Match(RustExprMatch),
    MethodCall(RustExprMethodCall),
    Paren(RustExprParen),
    Path(RustExprPath),
    Range(RustExprRange),
    Reference(RustExprReference),
    Repeat(RustExprRepeat),
    Return(RustExprReturn),
    Struct(RustExprStruct),
    Try(RustExprTry),
    TryBlock(RustExprTryBlock),
    Tuple(RustExprTuple),
    Unary(RustExprUnary),
    Unsafe(RustExprUnsafe),
    Verbatim(RustTokenStream),
    While(RustExprWhile),
    Yield(RustExprYield),
}

#[derive(Debug, Clone)]
pub struct RustExprArray {
    pub attrs: Vec<Attribute>,
    pub elems: Vec<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprAssign {
    pub attrs: Vec<Attribute>,
    pub left: Box<ExprRef>,
    pub right: Box<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprAsync {
    pub attrs: Vec<Attribute>,
    pub capture: bool,
    pub stmts: Vec<StmtsRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprAwait {
    pub attrs: Vec<Attribute>,
    pub base: Box<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprBinary {
    pub attrs: Vec<Attribute>,
    pub left: Box<ExprRef>,
    pub op: BinOp,
    pub right: Box<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprBlock {
    pub attrs: Vec<Attribute>,
    pub label: Option<Label>,
    pub stmts: Vec<StmtsRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprBreak {
    pub attrs: Vec<Attribute>,
    pub label: Option<Lifetime>,
    pub expr: Option<Box<ExprRef>>,
}
#[derive(Debug, Clone)]
pub struct RustExprCall {
    pub attrs: Vec<Attribute>,
    pub func: Box<ExprRef>,
    pub args: Vec<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprCast {
    pub attrs: Vec<Attribute>,
    pub expr: Box<ExprRef>,
    pub ty: Box<Type>,
}
#[derive(Debug, Clone)]
pub struct RustExprClosure {
    pub attrs: Vec<Attribute>,
    pub lifetimes: Option<BoundLifetimes>,
    pub constness: bool,
    pub movability: bool,
    pub asyncness: bool,
    pub capture: bool,
    pub inputs: Vec<Pat>,
    pub output: ReturnType,
    pub body: Box<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprConst {
    pub attrs: Vec<Attribute>,
    pub block: Vec<StmtsRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprContinue {
    pub attrs: Vec<Attribute>,
    pub label: Option<Lifetime>,
}
#[derive(Debug, Clone)]
pub struct RustExprField {
    pub attrs: Vec<Attribute>,
    pub base: Box<ExprRef>,
    pub member: Member,
}
#[derive(Debug, Clone)]
pub struct RustExprForLoop {
    pub attrs: Vec<Attribute>,
    pub label: Option<Label>,
    pub pat: Box<Pat>,
    pub expr: Box<ExprRef>,
    pub body: Vec<StmtsRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprGroup {
    pub attrs: Vec<Attribute>,
    pub expr: Box<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprIf {
    pub attrs: Vec<Attribute>,
    pub cond: Box<ExprRef>,
    pub then_branch: Vec<StmtsRef>,
    pub else_branch: Option<Box<ExprRef>>,
}
#[derive(Debug, Clone)]
pub struct RustExprIndex {
    pub attrs: Vec<Attribute>,
    pub expr: Box<ExprRef>,
    pub index: Box<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprInfer {
    pub attrs: Vec<Attribute>,
}
#[derive(Debug, Clone)]
pub struct RustExprLet {
    pub attrs: Vec<Attribute>,
    pub pat: Box<Pat>,
    pub expr: Box<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprLit {
    pub attrs: Vec<Attribute>,
    pub lit: Lit,
}
#[derive(Debug, Clone)]
pub struct RustExprLoop {
    pub attrs: Vec<Attribute>,
    pub label: Option<Label>,
    pub body: Vec<StmtsRef>,
}
// #[derive(Debug, Clone)]
// pub struct RustExprMacro {
//     pub attrs: Vec<Attribute>,
//     pub mac: Macro,
// }
#[derive(Debug, Clone)]
pub struct RustExprMatch {
    pub attrs: Vec<Attribute>,
    pub expr: Box<ExprRef>,
    pub arms: Vec<RustArm>,
}
#[derive(Debug, Clone)]
pub struct RustArm {
    pub attrs: Vec<Attribute>,
    // TODO Pat can contain an expression and thus an item - need to check whether items can really be defined *anywhere*
    pub pat: Pat,
    pub guard: Option<Box<ExprRef>>,
    pub body: Box<ExprRef>,
    pub comma: bool,
}
#[derive(Debug, Clone)]
pub struct RustExprMethodCall {
    pub attrs: Vec<Attribute>,
    pub receiver: Box<ExprRef>,
    pub method: syn::Ident,
    pub turbofish: Option<AngleBracketedGenericArguments>,
    pub args: Vec<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprParen {
    pub attrs: Vec<Attribute>,
    pub expr: Box<ExprRef>,
}
// TODO this type is unnecessary, can just use ExprPath directly
#[derive(Debug, Clone)]
pub struct RustExprPath {
    pub attrs: Vec<Attribute>,
    pub qself: Option<QSelf>,
    pub path: syn::Path,
}
#[derive(Debug, Clone)]
pub struct RustExprRange {
    pub attrs: Vec<Attribute>,
    pub start: Option<Box<ExprRef>>,
    pub limits: RangeLimits,
    pub end: Option<Box<ExprRef>>,
}
#[derive(Debug, Clone)]
pub struct RustExprReference {
    pub attrs: Vec<Attribute>,
    pub mutability: bool,
    pub expr: Box<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprRepeat {
    pub attrs: Vec<Attribute>,
    pub expr: Box<ExprRef>,
    pub len: Box<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprReturn {
    pub attrs: Vec<Attribute>,
    pub expr: Option<Box<ExprRef>>,
}
#[derive(Debug, Clone)]
pub struct RustExprStruct {
    pub attrs: Vec<Attribute>,
    pub qself: Option<QSelf>,
    pub path: syn::Path,
    pub fields: Vec<RustFieldValue>,
    pub dot2_token: bool,
    pub rest: Option<Box<ExprRef>>,
}
#[derive(Debug, Clone)]
pub struct RustFieldValue {
    pub attrs: Vec<Attribute>,
    pub member: Member,
    pub colon_token: bool,
    pub expr: ExprRef,
}
#[derive(Debug, Clone)]
pub struct RustExprTry {
    pub attrs: Vec<Attribute>,
    pub expr: Box<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprTryBlock {
    pub attrs: Vec<Attribute>,
    pub block: Vec<StmtsRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprTuple {
    pub attrs: Vec<Attribute>,
    pub elems: Vec<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprUnary {
    pub attrs: Vec<Attribute>,
    pub op: UnOp,
    pub expr: Box<ExprRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprUnsafe {
    pub attrs: Vec<Attribute>,
    // pub block: Vec<StmtsRef>,
    // TODO we use RustExprBlock even though unsafe blocks do not have attrs or lables, because it allows easy reuse of handle_expr_block, but really we should just separate handle_expr_block out into two nested fns to handle both cases
    pub block: RustExprBlock,
}
#[derive(Debug, Clone)]
pub struct RustTokenStream {
    syn: TokenStream,
}
#[derive(Debug, Clone)]
pub struct RustExprWhile {
    pub attrs: Vec<Attribute>,
    pub label: Option<Label>,
    pub cond: Box<ExprRef>,
    pub body: Vec<StmtsRef>,
}
#[derive(Debug, Clone)]
pub struct RustExprYield {
    syn: ExprYield,
}
