#![allow(dead_code)]

use std::{fs, path::PathBuf};

use syn::{
    Expr, GenericParam, ImplItem, Item, ItemConst, ItemEnum, ItemImpl, ItemStruct, ItemTrait,
    ItemUse, Local, Meta, Signature, Stmt, StmtMacro, Type, UseTree, Visibility,
};
use tracing::debug;
use update_definitons::{FnInfoSyn, ItemV2};

use crate::{extract_modules::tree_parsing_for_boilerplate, RustPathSegment, PRELUDE_MODULE_PATH};

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
        use_mapping: item_paths,
    }
}

// Actual definitions (only use at top level)
#[derive(Debug, Clone)]
pub enum ItemActual {
    StructOrEnum(ItemDefinition),
    Fn(FnInfo),
    Const(ConstDef),
    Trait(RustTraitDefinition),
    // Should never be handled, only used for empty initialisation
    // TODO replace use with Option?
    None,
    // Mod(RustMod),
    // Impl(RustMod),
    // Use(RustUse),
}

// References to top level deifinitions (used inside Mod's, Fn bodies)
#[derive(Debug, Clone)]
pub enum ItemRef {
    StructOrEnum(usize),
    Fn(usize),
    Const(usize),
    Trait(usize),
    Impl(usize),
    Mod(RustMod),
    Use(RustUse),
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
    pub use_mapping: Vec<(String, Vec<String>)>,
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
        items: &[ItemActual],
        use_private: bool,
        name: &str,
    ) -> Option<usize> {
        self.items.iter().find_map(|item| match item {
            ItemRef::StructOrEnum(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemActual::StructOrEnum(def) => def,
                    _ => todo!(),
                };
                (def.ident == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Fn(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemActual::Fn(fn_info) => fn_info,
                    _ => todo!(),
                };
                (def.ident == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Const(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemActual::Const(def) => def,
                    _ => todo!(),
                };
                (def.name == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Trait(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemActual::Trait(def) => def,
                    _ => todo!(),
                };
                (def.name == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Mod(_) => todo!(),
            ItemRef::Impl(_) => todo!(),
            ItemRef::Use(_) => todo!(),
        })
    }
    pub fn path_starts_with_sub_module(&self, use_private: bool, ident: &str) -> bool {
        self.items.iter().any(|item| match item {
            ItemRef::Mod(rust_mod) => {
                rust_mod.module_path[0] == ident && (use_private || rust_mod.pub_)
            }
            _ => false,
        })
    }

    pub fn item_defined_in_module2(
        &self,
        items: &[ItemV2],
        use_private: bool,
        name: &str,
    ) -> Option<usize> {
        self.items.iter().find_map(|item| match item {
            ItemRef::StructOrEnum(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemV2::StructOrEnum(def) => def,
                    _ => todo!(),
                };
                (def.ident == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Fn(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemV2::Fn(fn_info) => fn_info,
                    _ => todo!(),
                };
                (def.ident == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Const(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemV2::Const(def) => def,
                    _ => todo!(),
                };
                (def.name == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Trait(index) => {
                let item = &items[*index];
                let def = match item {
                    ItemV2::Trait(def) => def,
                    _ => todo!(),
                };
                (def.name == name && (use_private || def.is_pub)).then_some(*index)
            }
            ItemRef::Mod(_) => todo!(),
            ItemRef::Impl(_) => todo!(),
            ItemRef::Use(_) => todo!(),
        })
    }
}

#[derive(Debug, Clone)]
struct RustImplV1 {
    pub syn: ItemImpl,
    pub items: Vec<ImplItemV1>,
}
#[derive(Debug, Clone)]
enum ImplItemV1 {
    Fn(FnInfo),
    Const(ConstDef),
}

fn item_to_rust_item(_item: Item) -> ItemRef {
    todo!()
}

// Returns a Vec of item (struct/enum/trait/const) defs, and a nested/AST of `ItemV1`s where a ItemV1 struct/enum/trait/const is simply an index into the Vec.
// NOTE because a FnInfo item itself can contain other items, including mods etc, it is not possible to keep struct/enum/trait/const's in a Vec, and the other items in a tree with indexes into the Vec
// I think the only option is to either keep the tree and the indexes must instead be a Vec of indexes (requiring allocation and keeping track of the indexes of parents), of keep everything in a flat structure. This means we need to be able to identify the crate level items somehow, eg by putting them in a RustMod

// When called for a top level module like a crate, the returned ItemRef's are the top level Items of the crate
// NOTE when extract_modules is called at the top level (eg crate items) we obviously want it to return Vec<ItemActual> since Vec<ItemRef> would not make sense as the refs wouldn't have anything to point to. However, when extract_modules is called for a submodule, we need to Vec<ItemRef> so the items can be stored in the RustMod, and for the actual items to be pushed directly to the top level Vec, to ensure we can record the correct index.
pub fn extract_modules2(
    _module_level_items: bool,
    items: &Vec<Item>,
    // Same as `global_data.crate_path`, used for prepending module filepaths, except we don't have a `GlobalData` yet so we pass it in directly
    // None if we are extracting data for a single file or snippet, rather than an actual crate (so no `mod foo` allowed)
    // TODO crate_path might use hiphens instead of underscore as a word seperator, so need to ensure it is only used for file paths, and not module paths
    crate_path: &Option<PathBuf>,
    current_path: &mut Vec<String>,
    actual_item_defs: &mut Vec<ItemActual>,
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
    let mut module_itemrefs = Vec::new();
    let _top_mod = RustMod {
        pub_: false,
        module_path: current_path.clone(),
        items: Vec::new(),
    };

    // TODO the code for eg module.item_definitions.push(...) is a duplicated also for scope.item_definitons.push(...). Remove this duplication.
    for item in items {
        match item {
            Item::Const(item_const) => {
                let const_name = item_const.ident.to_string();

                let is_pub = match item_const.vis {
                    Visibility::Public(_) => true,
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => false,
                };
                actual_item_defs.push(ItemActual::Const(ConstDef {
                    name: const_name,
                    is_pub,
                    syn_object: item_const.clone(),
                }));
                module_itemrefs.push(ItemRef::Const(actual_item_defs.len() - 1))
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
                            let tokens = format!("({})", meta_list.tokens);
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
                actual_item_defs.push(ItemActual::StructOrEnum(ItemDefinition {
                    ident: enum_name,
                    is_copy,
                    is_pub,
                    generics,
                    struct_or_enum_info: StructOrEnumDefitionInfo::Enum(EnumDefinitionInfo {
                        // members: members_for_scope,
                        syn_object: item_enum.clone(),
                    }),
                    impl_block_ids: Vec::new(),
                }));
                module_itemrefs.push(ItemRef::StructOrEnum(actual_item_defs.len() - 1));
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
                    .map(|stmt| match stmt {
                        Stmt::Local(local) => StmtsRef::Local(local),
                        Stmt::Item(item) => StmtsRef::Item(item_to_rust_item(item)),
                        Stmt::Expr(expr, semi) => StmtsRef::Expr(expr, semi.is_some()),
                        Stmt::Macro(stmt_macro) => StmtsRef::Macro(stmt_macro),
                    })
                    .collect();

                actual_item_defs.push(ItemActual::Fn(FnInfo {
                    ident: item_fn.sig.ident.to_string(),
                    is_pub,
                    generics,
                    signature: item_fn.sig.clone(),
                    // syn: FnInfoSyn::Standalone(item_fn.clone()),
                    stmts: rust_stmts,
                    syn: FnInfoSyn::Standalone(item_fn.clone()),
                }));
                module_itemrefs.push(ItemRef::Fn(actual_item_defs.len() - 1));
            }
            Item::ForeignMod(_) => todo!(),
            Item::Impl(item_impl) => {
                // Extract modules from impl blocks

                let mut rust_impl_items = Vec::new();
                for item in &item_impl.items {
                    match item {
                        ImplItem::Const(_) => todo!(),
                        ImplItem::Fn(impl_item_fn) => {
                            // TODO dedupe with Item::Fn
                            let generics = impl_item_fn
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

                            let is_pub = match impl_item_fn.vis {
                                Visibility::Public(_) => true,
                                Visibility::Restricted(_) => todo!(),
                                Visibility::Inherited => false,
                            };

                            let stmts = impl_item_fn
                                .block
                                .stmts
                                .clone()
                                .into_iter()
                                .map(|stmt| match stmt {
                                    Stmt::Local(local) => StmtsRef::Local(local),
                                    Stmt::Item(item) => StmtsRef::Item(item_to_rust_item(item)),
                                    Stmt::Expr(expr, semi) => StmtsRef::Expr(expr, semi.is_some()),
                                    Stmt::Macro(stmt_macro) => StmtsRef::Macro(stmt_macro),
                                })
                                .collect();

                            rust_impl_items.push(ImplItemV1::Fn(FnInfo {
                                ident: impl_item_fn.sig.ident.to_string(),
                                is_pub,
                                generics,
                                signature: impl_item_fn.sig.clone(),
                                stmts,
                                syn: FnInfoSyn::Impl(impl_item_fn.clone()),
                            }))
                        }
                        ImplItem::Type(_) => todo!(),
                        ImplItem::Macro(_) => todo!(),
                        ImplItem::Verbatim(_) => todo!(),
                        _ => todo!(),
                    }
                }
            }
            Item::Macro(_) => {}
            // We have already split up the modules in individual `ModuleData`s (which we are currently iterating through) so should ignore `Item::Mod`s
            Item::Mod(item_mod) => {
                let pub_ = match item_mod.vis {
                    Visibility::Public(_) => true,
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => false,
                };

                let mut rust_mod = RustMod {
                    pub_,
                    module_path: current_path.clone(),
                    items: Vec::new(),
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

                // let mut partial_module_data = ModuleDataFirstPass::new(
                //     item_mod.ident.to_string(),
                //     // parent_name,
                //     current_path.clone(),
                // );

                // NOTE we do the `modules.push(ModuleData { ...` below because we need to get the module items from the different content/no content branches
                if let Some(content) = &item_mod.content {
                    // partial_module_data.items.clone_from(&content.1);
                    // modules.push(partial_module_data);

                    // TODO how does `mod bar { mod foo; }` work?
                    rust_mod.items = extract_modules2(
                        true,
                        &content.1,
                        crate_path,
                        current_path,
                        actual_item_defs,
                    );
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
                    rust_mod.items = extract_modules2(
                        true,
                        &file.items,
                        crate_path,
                        current_path,
                        actual_item_defs,
                    );
                } else {
                    panic!("`mod foo` is not allowed in files/modules/snippets, only crates")
                }
                current_path.pop();

                module_itemrefs.push(ItemRef::Mod(rust_mod));
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
                actual_item_defs.push(ItemActual::StructOrEnum(ItemDefinition {
                    ident: item_struct.ident.to_string(),
                    is_pub,
                    is_copy,
                    generics,
                    struct_or_enum_info: StructOrEnumDefitionInfo::Struct(StructDefinitionInfo {
                        // fields,
                        syn_object: item_struct.clone(),
                    }),
                    impl_block_ids: Vec::new(),
                }));
                module_itemrefs.push(ItemRef::StructOrEnum(actual_item_defs.len() - 1));
            }
            Item::Trait(item_trait) => {
                let is_pub = match item_trait.vis {
                    Visibility::Public(_) => true,
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => false,
                };
                actual_item_defs.push(ItemActual::Trait(RustTraitDefinition {
                    name: item_trait.ident.to_string(),
                    is_pub,
                    syn: item_trait.clone(),
                }));
                module_itemrefs.push(ItemRef::Trait(actual_item_defs.len() - 1));
            }
            Item::TraitAlias(_) => todo!(),
            Item::Type(_) => todo!(),
            Item::Union(_) => todo!(),
            Item::Use(item_use) => {
                // TODO we are adding all use stmts the the module use mappings rather than accounting for when we are not at the top level so the stmts should be added to the scope? Also does `resolve_path()` account for the difference?
                // let module = modules.get_mut(current_path);
                module_itemrefs.push(ItemRef::Use(handle_item_use2(item_use)))
            }
            Item::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }

    module_itemrefs
}

// pub fn make_item_definitions(modules: Vec<ModuleDataFirstPass>) -> Vec<ModuleData> {
//     // TODO the code for eg module.item_definitions.push(...) is a duplicated also for scope.item_definitons.push(...). Remove this duplication.

//     // This is because parse_types_for_populate_item_definitions needs a access to .pub_definitions etc in global_data from `extract_data()` but we are taking an immutable ref first
//     // We also need it for looking up trait definitions

//     let mut new_modules = Vec::new();
//     for module_first_pass in modules {
//         debug_span!(
//             "extract_data_populate_item_definitions module: {:?}",
//             module_path = ?module_first_pass.path
//         );

//         let mut module = ModuleData {
//             name: module_first_pass.name,
//             // parent_name: module,
//             path: module_first_pass.path,
//             pub_submodules: module_first_pass.pub_submodules,
//             private_submodules: module_first_pass.private_submodules,
//             pub_use_mappings: module_first_pass.pub_use_mappings,
//             private_use_mappings: module_first_pass.private_use_mappings,
//             resolved_mappings: Vec::new(),
//             various_definitions: VariousDefintions::default(),
//             items: module_first_pass.items,
//             scoped_various_definitions: Vec::new(),
//             scoped_syn_impl_items: Vec::new(),
//         };

//         // TODO Gymnastics to reconcile needing to mutate 4 different vectors which are stored differently for modules and scopes. Should probably have `module.various_defs` and `scope.various_defs` fields
//         let mut var_defs = VariousDefintions::default();
//         let items = module.items.clone();
//         let module_path = module.path.clone();
//         let mut scope_id = Vec::new();

//         let mut scope_count = 0;
//         for item in &items {
//             populate_item_definitions_items_individual_item(
//                 item,
//                 &module_path,
//                 &mut var_defs,
//                 &mut module,
//                 &mut scope_id,
//                 &mut scope_count,
//             )
//         }
//         // module.various_definitions.fn_info.extend(var_defs.fn_info);
//         // module
//         //     .various_definitions
//         //     .item_definitons
//         //     .extend(var_defs.item_definitons);
//         // module.various_definitions.consts.extend(var_defs.consts);
//         // module
//         //     .various_definitions
//         //     .trait_definitons
//         //     .extend(var_defs.trait_definitons);
//         module.various_definitions = var_defs;
//         new_modules.push(module);
//     }
//     new_modules
// }

fn populate_item_definitions_stmts(
    stmts: &[Stmt],
    // global_data: &GlobalData,
    module_path: &[String],
    _current_scope_various_defs: &mut VariousDefintions,
    module: &mut ModuleData,
    scope_id: &mut Vec<usize>,
) {
    let mut scope_count = 0;

    for stmt in stmts {
        match stmt {
            Stmt::Local(local) => {
                let init = local.init.as_ref().unwrap();
                populate_item_definitions_expr(
                    &init.expr,
                    // global_data,
                    module_path,
                    // current_scope_various_defs,
                    module,
                    scope_id,
                    &mut scope_count,
                    false,
                );
            }
            Stmt::Item(_item) => {
                // populate_item_definitions_items_individual_item(
                //     item,
                //     // global_data,
                //     module_path,
                //     current_scope_various_defs,
                //     module,
                //     scope_id,
                //     &mut scope_count,
                // )
            }
            Stmt::Expr(expr, _) => populate_item_definitions_expr(
                expr,
                // global_data,
                module_path,
                // current_scope_various_defs,
                module,
                scope_id,
                &mut scope_count,
                false,
            ),
            Stmt::Macro(stmt_macro) => {
                if stmt_macro.mac.path.segments.len() == 1
                    && stmt_macro.mac.path.segments.first().unwrap().ident == "assert"
                {
                    let input = stmt_macro.mac.tokens.clone().to_string();
                    let condition_expr = syn::parse_str::<syn::Expr>(&input).unwrap();
                    populate_item_definitions_expr(
                        &condition_expr,
                        // global_data,
                        module_path,
                        // current_scope_various_defs,
                        module,
                        scope_id,
                        &mut scope_count,
                        false,
                    )
                } else {
                    dbg!(&stmt_macro.mac);
                    todo!();
                }
            }
        }
    }
}

fn populate_item_definitions_expr(
    expr: &Expr,
    // global_data: &GlobalData,
    module_path: &[String],
    // various_defs: &mut VariousDefintions,
    module: &mut ModuleData,
    scope_id: &mut Vec<usize>,
    // NOTE when we are iterating through some stmts for a given scope like a fn, we need to be able to increment the scope count each time we come across an expression that create a new child scope eg a block. So for a fn body with 3 blocks, by the time we reach the final block, the scope_count will have been incremented to 2 (0 based) so we can then push the scope_count onto scope_id to create a new scope for processing the statement in that block.
    scope_count: &mut usize,
    // In some cases, eg the body of a match arm like `Foo(x) => x`, we need to ensure a new scope is created even though we wouldn't normally create one for the give `Expr` variant, ie an `Expr::Path` in this case.
    // NOTE both here and in the JS parsing code, we create a new scope even if we don't need to eg for match arms like `Foo => 5`.
    force_new_scope: bool,
) {
    fn forced_inc_scope_count_and_id_and_push_empty_scope(
        force_new_scope: bool,
        scope_count: &mut usize,
        scope_id: &mut Vec<usize>,
        module: &mut ModuleData,
    ) {
        if force_new_scope {
            *scope_count += 1;
            scope_id.push(*scope_count);

            let empty_various_defs = VariousDefintions::default();
            module
                .scoped_various_definitions
                .push((scope_id.clone(), empty_various_defs));
        }
    }
    fn drop_forced_empty_scope(force_new_scope: bool, scope_id: &mut Vec<usize>) {
        if force_new_scope {
            scope_id.pop();
        }
    }

    // We only want to create a new scope for the below expressions
    // TODO IMPORTANT call `make_forced_empty_scope` for all variants
    match expr {
        Expr::Array(_) => {}
        Expr::Assign(_) => {}
        Expr::Async(expr_async) => {
            // This is identical to the handling of Expr::Block but we have to get the block from the `ExprAsync` first
            *scope_count += 1;
            scope_id.push(*scope_count);

            let mut scoped_various_defs = VariousDefintions::default();
            populate_item_definitions_stmts(
                &expr_async.block.stmts,
                // global_data,
                module_path,
                &mut scoped_various_defs,
                module,
                scope_id,
            );

            module.scoped_various_definitions.push((
                scope_id.clone(),
                scoped_various_defs,
                // Vec::new(),
            ));

            scope_id.pop();
        }
        Expr::Await(_) => {}
        Expr::Binary(_) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            // TODO call populate_item_definitions_expr for side
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Block(expr_block) => {
            *scope_count += 1;
            scope_id.push(*scope_count);

            let mut scoped_various_defs = VariousDefintions::default();
            populate_item_definitions_stmts(
                &expr_block.block.stmts,
                // global_data,
                module_path,
                &mut scoped_various_defs,
                module,
                scope_id,
            );

            module.scoped_various_definitions.push((
                scope_id.clone(),
                scoped_various_defs,
                // Vec::new(),
            ));

            scope_id.pop();
        }
        Expr::Break(_) => {}
        Expr::Call(_) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            // TODO call populate_item_definitions_expr for each argument?
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Cast(_) => {}
        Expr::Closure(expr_closure) => {
            populate_item_definitions_expr(
                &expr_closure.body,
                // global_data,
                module_path,
                module,
                scope_id,
                scope_count,
                true,
            );
        }
        Expr::Const(_) => {}
        Expr::Continue(_) => {}
        Expr::Field(_) => {}
        Expr::ForLoop(expr_for_loop) => {
            *scope_count += 1;
            scope_id.push(*scope_count);

            let mut scoped_various_defs = VariousDefintions::default();
            populate_item_definitions_stmts(
                &expr_for_loop.body.stmts,
                // global_data,
                module_path,
                &mut scoped_various_defs,
                module,
                scope_id,
            );

            module.scoped_various_definitions.push((
                scope_id.clone(),
                scoped_various_defs,
                // Vec::new(),
            ));

            scope_id.pop();
        }
        Expr::Group(_) => {}
        Expr::If(_) => {}
        Expr::Index(_) => {}
        Expr::Infer(_) => {}
        Expr::Let(_) => {}
        Expr::Lit(_) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Loop(_) => {}
        Expr::Macro(_) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Match(expr_match) => {
            // We wouldn't normally create a new scope for a match expression but if it is the body of eg a closure or a another match expression's arm body, then force_new_scope will be true and we must create an empty scope (NOTE this is different to the arm body scopes which are created below, this is a single empty scope for the entire match expression)
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );

            populate_item_definitions_expr(
                &expr_match.expr,
                // global_data,
                module_path,
                module,
                scope_id,
                scope_count,
                false,
            );
            // TODO We count in reverse to match the JS parsing but should probably try and fix this so we don't have to remember that match arm body scopes are counted backwards
            for arm in expr_match.arms.iter().rev() {
                // In this case, even if the arm body is simply a path like `x`, we still need to create a scope because the `x` can be an argument of eg the enum variant for this match arm.
                populate_item_definitions_expr(
                    &arm.body,
                    // global_data,
                    module_path,
                    module,
                    scope_id,
                    scope_count,
                    true,
                );
            }
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::MethodCall(expr_method_call) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            populate_item_definitions_expr(
                &expr_method_call.receiver,
                // global_data,
                module_path,
                module,
                scope_id,
                scope_count,
                false,
            );
            for expr in &expr_method_call.args {
                populate_item_definitions_expr(
                    expr,
                    // global_data,
                    module_path,
                    module,
                    scope_id,
                    scope_count,
                    false,
                );
            }
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Paren(_) => {}
        Expr::Path(_) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Range(_) => {}
        Expr::Reference(_) => {
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
                module,
            );
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Repeat(_) => {}
        Expr::Return(_) => {}
        Expr::Struct(_) => {}
        Expr::Try(_) => {}
        Expr::TryBlock(_) => {}
        Expr::Tuple(_) => {}
        Expr::Unary(expr_unary) => {
            // TODO I think it is correct that we don't need to call `forced_inc_scope_count_and_id_and_push_empty_scope` and can just pass `force_new_scope` down to the target expression to create a new scope if necessary? If this is correct then `Expr::Reference` should be the same as similarly to unary you can't have a reference with no target
            // forced_inc_scope_count_and_id_and_push_empty_scope(
            //     force_new_scope,
            //     scope_count,
            //     scope_id,
            //     module,
            // );
            populate_item_definitions_expr(
                &expr_unary.expr,
                module_path,
                module,
                scope_id,
                scope_count,
                force_new_scope,
                // false,
            );
            // drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Unsafe(_) => {}
        Expr::Verbatim(_) => {}
        Expr::While(_) => {}
        Expr::Yield(_) => {}
        _ => {}
    }
}

#[derive(Debug, Clone)]
pub struct StructDefinitionInfo {
    // pub fields: StructFieldInfo,
    pub syn_object: ItemStruct,
}

#[derive(Debug, Clone)]
pub struct EnumDefinitionInfo {
    // pub members: Vec<EnumVariantInfo>,
    pub syn_object: ItemEnum,
}

#[derive(Debug, Clone)]
pub enum StructOrEnumDefitionInfo {
    Struct(StructDefinitionInfo),
    Enum(EnumDefinitionInfo),
}

/// Similar to StructOrEnum which gets used in RustType, but is for storing info about the actual item definition, rather than instances of, so eg we don't need to be able to store resolved generics. Minor differences but making distinct type helps with reasoning about the different use cases.
/// Just structs and enums or should we include functions?
#[derive(Debug, Clone)]
pub struct ItemDefinition {
    pub ident: String,
    // NOTE we don't need to store the module path because module level `ItemDefinition`s are stored within modules so we will already know the module path
    // module_path: Option<Vec<String>>,
    pub is_copy: bool,
    pub is_pub: bool,
    // TODO do we need to know eg bounds for each generic?
    pub generics: Vec<String>,
    // syn_object: StructOrEnumSynObject,
    pub struct_or_enum_info: StructOrEnumDefitionInfo,
    // impl_blocks: Vec<ItemDefintionImpls>,
    /// (unique impl id)
    pub impl_block_ids: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct RustTraitDefinition {
    pub name: String,
    pub is_pub: bool,
    // impl_items:
    pub syn: ItemTrait,
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
    // /// Update generics based on types of args
    // ///
    // /// For all the generics of the struct/enum...
    // /// ...for enum check if any of the arguments to any of the variants are the generic type...
    // /// ...(if) we found a generic so now we need to find the type of the argument being passed and we will have the full `MyEnum<FoundGeneric>` type
    // fn attempt_to_resolve_generics(
    //     &self,
    //     field_or_variant_name: &String,
    //     args: &Vec<(JsExpr, RustType)>,
    // ) -> Vec<RustTypeParam> {
    //     let mut possibly_resolved_generics = Vec::new();
    //     for generic in &self.generics {
    //         match &self.struct_or_enum_info {
    //             StructOrEnumDefitionInfo::Struct(_) => todo!(),
    //             StructOrEnumDefitionInfo::Enum(enum_def_info) => {
    //                 let item_enum = &enum_def_info.syn_object;
    //                 // ...for enum check if any of the arguments to any of the variants are the generic type...
    //                 for v in &item_enum.variants {
    //                     if v.ident == field_or_variant_name {
    //                         match &v.fields {
    //                             Fields::Named(fields_named) => {
    //                                 for (i, field) in fields_named.named.iter().enumerate() {
    //                                     match &field.ty {
    //                                         Type::Path(type_path) => {
    //                                             if type_path.path.segments.first().unwrap().ident
    //                                                 == generic
    //                                             {
    //                                                 // ...we found a generic so now we need to find the type of the argument being passed and we will have the full `MyEnum<FoundGeneric>` type
    //                                                 let (_js_expr, rust_type) = args[i].clone();
    //                                                 possibly_resolved_generics.push(
    //                                                     RustTypeParam {
    //                                                         name: generic.clone(),
    //                                                         type_: RustTypeParamValue::RustType(
    //                                                             Box::new(rust_type),
    //                                                         ),
    //                                                     },
    //                                                 );
    //                                                 continue;
    //                                             }
    //                                         }
    //                                         Type::Verbatim(_) => todo!(),
    //                                         _ => todo!(),
    //                                     }
    //                                 }
    //                             }
    //                             Fields::Unnamed(fields_unnamed) => {
    //                                 for (i, field) in fields_unnamed.unnamed.iter().enumerate() {
    //                                     match &field.ty {
    //                                         Type::Path(type_path) => {
    //                                             if type_path.path.segments.first().unwrap().ident
    //                                                 == generic
    //                                             {
    //                                                 // ...we found a generic so now we need to find the type of the argument being passed and we will have the full `MyEnum<FoundGeneric>` type
    //                                                 let (_js_expr, rust_type) = args[i].clone();
    //                                                 possibly_resolved_generics.push(
    //                                                     RustTypeParam {
    //                                                         name: generic.clone(),
    //                                                         type_: RustTypeParamValue::RustType(
    //                                                             Box::new(rust_type),
    //                                                         ),
    //                                                     },
    //                                                 );
    //                                                 continue;
    //                                             }
    //                                         }
    //                                         Type::Verbatim(_) => todo!(),
    //                                         _ => todo!(),
    //                                     }
    //                                 }
    //                             }
    //                             Fields::Unit => todo!(),
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //         possibly_resolved_generics.push(RustTypeParam {
    //             name: generic.clone(),
    //             type_: RustTypeParamValue::Unresolved,
    //         });
    //     }
    //     possibly_resolved_generics
    // }
}

#[derive(Debug, Clone)]
pub struct ModuleData {
    pub name: String,
    // parent_name: Option<String>,
    /// NOTE the path includes the name of the module, eg the path to the crate module is ["crate"] not [].
    pub path: Vec<String>,
    // pub_definitions: Vec<String>,
    // private_definitions: Vec<String>,
    pub pub_submodules: Vec<String>,
    pub private_submodules: Vec<String>,
    /// (snake case item name, snake case use path)
    pub pub_use_mappings: Vec<(String, Vec<String>)>,
    pub private_use_mappings: Vec<(String, Vec<String>)>,
    /// Same format as use mapping but has absolute module path
    /// (snake case item name, snake case absolute module path)
    pub resolved_mappings: Vec<(String, Vec<String>)>,
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
    // // fn_info: Vec<(String, Vec<String>)>,
    // pub fn_info: Vec<FnInfo>,
    // pub item_definitons: Vec<ItemDefinition>,
    // /// (name, type, syn const)
    // pub consts: Vec<ConstDef>,
    // pub trait_definitons: Vec<RustTraitDefinition>,
    pub various_definitions: VariousDefintions,

    // We need this for extract_data_populate_item_definitions which happens after the modules ModuleData has been created by extract_data, but is populating the `ItemDefiitions` etc, and needs access to the original items in the module for this
    pub items: Vec<Item>,

    // (scope number, definitions)
    // scope number is eg [3,4,2] where this is the 3rd scope that appears within the module (not nested inside another scope, eg if the first 3 items are fns, this would be the body block of the 3rd fn, regardless of how many nested scoped there are in the first two fns), the 4th scope within that scope (same rules as before), and then finally the second scope within that scope
    // scoped_various_definitions: Vec<(Vec<usize>, VariousDefintions, Vec<RustImplBlockSimple>)>,
    pub scoped_various_definitions: Vec<(Vec<usize>, VariousDefintions)>,
    pub scoped_syn_impl_items: Vec<(Vec<usize>, ItemImpl)>,
}
impl ModuleData {
    pub fn item_defined_in_module(&self, use_private: bool, item: &str) -> bool {
        // let mut definitions = self.pub_definitions.iter();
        // if use_private {
        //     definitions
        //         .chain(self.private_definitions.iter())
        //         .any(|definition| definition == item)
        // } else {
        //     definitions.any(|definition| definition == item)
        // }
        self.various_definitions
            .item_definitons
            .iter()
            .filter_map(|item_def| (use_private || item_def.is_pub).then_some(&item_def.ident))
            .chain(
                self.various_definitions
                    .fn_info
                    .iter()
                    .filter_map(|fn_info| {
                        (use_private || fn_info.is_pub).then_some(&fn_info.ident)
                    }),
            )
            .chain(
                self.various_definitions
                    .trait_definitons
                    .iter()
                    .filter_map(|trait_def| {
                        (use_private || trait_def.is_pub).then_some(&trait_def.name)
                    }),
            )
            .chain(
                self.various_definitions
                    .consts
                    .iter()
                    .filter_map(|const_| (use_private || const_.is_pub).then_some(&const_.name)),
            )
            .any(|name| name == item)
    }
    pub fn path_starts_with_sub_module(&self, use_private: bool, item: &str) -> bool {
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

#[derive(Debug, Clone, Default)]
pub struct VariousDefintions {
    pub fn_info: Vec<FnInfo>,
    pub item_definitons: Vec<ItemDefinition>,
    pub consts: Vec<ConstDef>,
    pub trait_definitons: Vec<RustTraitDefinition>,
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub name: String,
    pub is_pub: bool,
    pub syn_object: ItemConst,
}

// TODO attrs
/// Not just for methods, can also be an enum variant with no inputs
#[derive(Debug, Clone)]
pub struct FnInfo {
    pub ident: String,
    pub is_pub: bool,
    pub generics: Vec<String>,
    pub signature: Signature,
    // pub syn: FnInfoSyn,
    pub stmts: Vec<StmtsRef>,
    // TODO remove this, just legacy thing we need for now because it gets used in the JS parsing (I think)
    pub syn: FnInfoSyn,
}

#[derive(Debug, Clone)]
pub enum StmtsRef {
    Local(Local),
    Item(ItemRef),
    Expr(Expr, bool),
    Macro(StmtMacro),
}

pub trait ModuleMethods {
    fn lookup_item_definition_any_module_or_scope(
        &self,
        current_module_path: &[String],
        scope_id: &Option<Vec<usize>>,
        path: &[String],
    ) -> (Vec<String>, Option<Vec<usize>>, ItemDefinition);

    fn lookup_trait_definition_any_module<I>(
        &self,
        current_module_path: &[String],
        current_scope_id: &Option<Vec<usize>>,
        // path: &Vec<String>,
        path: I,
        // current_module: &Vec<String>,
    ) -> (Vec<String>, Option<Vec<usize>>, RustTraitDefinition)
    where
        // I: IntoIterator<Item = String>,
        I: IntoIterator,
        I::Item: AsRef<str>;
}
// impl ModuleMethods for Vec<ModuleData> {
//     /// NOTE to be used pre syn -> JS parsing, ie self.scopes won't have been populated
//     // -> (module path, scope id (even if we are in a scope so `Some` scope id is provided, the item being looked up might still be module level. Of course if None scope id is provided it is impossible for a Some scope id to be returned), item definition)
//     fn lookup_item_definition_any_module_or_scope(
//         &self,
//         current_module_path: &[String],
//         scope_id: &Option<Vec<usize>>,
//         path: &[String],
//     ) -> (Vec<String>, Option<Vec<usize>>, ItemDefinition) {
//         let (item_module_path, item_path, item_scope) = resolve_path(
//             true,
//             true,
//             path.iter()
//                 .map(|seg| RustPathSegment {
//                     ident: seg.clone(),
//                     turbofish: Vec::new(),
//                 })
//                 .collect::<Vec<_>>(),
//             self,
//             current_module_path,
//             current_module_path,
//             scope_id,
//         );
//         // dbg!(&item_module_path);
//         // dbg!(&item_path);
//         // dbg!(&item_scope);

//         let item_module = self.iter().find(|m| m.path == item_module_path).unwrap();

//         // TODO if the path is eg an associated fn, should we return the item or the fn? ie se or RustType?
//         let item_defintions = if let Some(scope_id) = &item_scope {
//             &item_module
//                 .scoped_various_definitions
//                 .iter()
//                 .find(|svd| &svd.0 == scope_id)
//                 .unwrap()
//                 .1
//                 .item_definitons
//         } else {
//             &item_module.various_definitions.item_definitons
//         };
//         let item_def = item_defintions
//             .iter()
//             .find(|se| se.ident == item_path[0].ident)
//             .unwrap();

//         (item_module_path, item_scope, item_def.clone())
//     }

//     fn lookup_trait_definition_any_module<I>(
//         &self,
//         current_module_path: &[String],
//         current_scope_id: &Option<Vec<usize>>,
//         // path: &Vec<String>,
//         path: I,
//         // current_module: &Vec<String>,
//     ) -> (Vec<String>, Option<Vec<usize>>, RustTraitDefinition)
//     where
//         // I: IntoIterator<Item = String>,
//         I: IntoIterator,
//         I::Item: AsRef<str>,
//     {
//         // dbg!("lookup_trait_definition_any_module");
//         let (item_module_path, item_path, item_scope) = resolve_path(
//             true,
//             true,
//             path.into_iter()
//                 .map(|seg| RustPathSegment {
//                     ident: seg.as_ref().to_string(),
//                     turbofish: Vec::new(),
//                 })
//                 .collect::<Vec<_>>(),
//             self,
//             current_module_path,
//             current_module_path,
//             current_scope_id,
//         );
//         let item_module = &self.iter().find(|m| m.path == item_module_path).unwrap();

//         let trait_definiton = if let Some(item_scope) = &item_scope {
//             let svd = item_module
//                 .scoped_various_definitions
//                 .iter()
//                 .find(|svd| &svd.0 == item_scope)
//                 .unwrap();
//             svd.1
//                 .trait_definitons
//                 .iter()
//                 .find(|trait_def| trait_def.name == item_path[0].ident)
//         } else {
//             item_module
//                 .various_definitions
//                 .trait_definitons
//                 .iter()
//                 .find(|t| t.name == item_path[0].ident)
//         };
//         (
//             item_module_path,
//             item_scope,
//             trait_definiton.unwrap().clone(),
//         )
//     }
// }

fn look_for_module_in_items(
    items: &[ItemRef],
    item_defs: &[ItemActual],
    module_path: &[String],
) -> Option<RustMod> {
    for item in items {
        match item {
            ItemRef::Fn(index) => {
                let item = &item_defs[*index];
                let fn_info = match item {
                    ItemActual::Fn(fn_info) => fn_info,
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

#[allow(clippy::too_many_arguments)]
/// -> (module path, item path, is scoped item, item index)
pub fn resolve_path(
    // look_for_scoped_vars: bool,
    // TODO can we combine this with `look_for_scoped_vars`?
    _look_for_scoped_items: bool,
    use_private_items: bool,
    mut segs: Vec<RustPathSegment>,
    module_items: &[ItemRef],
    // TODO replace GlobalData with `.modules` and `.scopes` to making setting up test cases easier
    // global_data: &GlobalData,
    items_defs: &[ItemActual],
    // scopes: &[GlobalDataScope],
    current_mod: &[String],
    // Only used to determine if current module is the original module
    orig_mod: &[String],
    scoped_items: &Vec<Vec<ItemRef>>,
) -> (Vec<String>, Vec<RustPathSegment>, bool, Option<usize>) {
    debug!(segs = ?segs, "get_path_without_namespacing");

    // TODO I don't think we need to pass in the module `ModuleData` if we are already passing the `current_module` module path we can just use that to look it up each time, which might be less efficient since we shouldn't need to lookup the module if we haven't changed modules (though I think we are pretty much always changing modules except for use statements?), but we definitely don't want to pass in both. Maybe only pass in `module: &ModuleData` and not `current_module`
    // assert!(current_module == &module.path);

    let module = look_for_module_in_items(module_items, items_defs, current_mod).unwrap();

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
    // let scoped_use_mapping = scopes
    //     .iter()
    //     .rev()
    //     .find_map(|s| s.use_mappings.iter().find(|u| u.0 == segs[0].ident));

    // let mut use_mappings = module.pub_use_mappings.iter();
    // let matched_use_mapping = if use_private_items || is_parent_or_same_module {
    //     use_mappings
    //         .chain(module.private_use_mappings.iter())
    //         .find(|use_mapping| use_mapping.0 == segs[0].ident)
    // } else {
    //     use_mappings.find(|use_mapping| use_mapping.0 == segs[0].ident)
    // };
    // dbg!(matched_use_mapping);
    let matched_use_mapping = module.items.iter().find_map(|item| match item {
        ItemRef::Use(rust_use) => rust_use.use_mapping.iter().find_map(|use_mapping| {
            (use_mapping.0 == segs[0].ident && (use_private || rust_use.pub_))
                .then_some(use_mapping.clone())
        }),
        _ => None,
    });

    // TODO can module shadow external crate names? In which case we need to look for modules first? I think we do this implicitly based on the order of the if statements below?
    // TODO actually look up external crates in Cargo.toml
    let external_crate_names = ["web_prelude"];
    let path_is_external_crate = external_crate_names.iter().any(|cn| cn == &segs[0].ident);

    // Look for scoped item
    let scoped_item = scoped_items.iter().rev().find_map(|s| {
        s.iter().find_map(|item| match item {
            ItemRef::StructOrEnum(index) => {
                let item = &items_defs[*index];
                let def = match item {
                    ItemActual::StructOrEnum(def) => def,
                    _ => todo!(),
                };
                (def.ident == segs[0].ident).then_some(*index)
            }
            ItemRef::Fn(index) => {
                let item = &items_defs[*index];
                let def = match item {
                    ItemActual::Fn(fn_info) => fn_info,
                    _ => todo!(),
                };
                (def.ident == segs[0].ident).then_some(*index)
            }
            ItemRef::Const(index) => {
                let item = &items_defs[*index];
                let def = match item {
                    ItemActual::Const(def) => def,
                    _ => todo!(),
                };
                (def.name == segs[0].ident).then_some(*index)
            }
            ItemRef::Trait(index) => {
                let item = &items_defs[*index];
                let def = match item {
                    ItemActual::Trait(def) => def,
                    _ => todo!(),
                };
                (def.name == segs[0].ident).then_some(*index)
            }
            ItemRef::Mod(_) => None,
            ItemRef::Impl(_) => None,
            ItemRef::Use(_) => None,
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
            module_items,
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
            module_items,
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
            module_items,
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
            module_items,
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
            module_items,
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
            module_items,
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
        {
            // TODO IMPORTANT we aren't meant to be handling these in get_path, they should be handled in the item def passes, not the JS parsing. add a panic!() here. NO not true, we will have i32, String, etc in closure defs, type def for var assignments, etc.
            // TODO properly encode "prelude_special_case" in a type rather than a String
            let item_index = module_items
                .iter()
                .find_map(|item_ref| match item_ref {
                    ItemRef::Mod(rust_mod) => {
                        if rust_mod.module_path == [PRELUDE_MODULE_PATH] {
                            rust_mod.items.iter().find_map(|item_ref| match item_ref {
                                ItemRef::StructOrEnum(index) => {
                                    let item = &items_defs[*index];
                                    match item {
                                        ItemActual::StructOrEnum(def) => {
                                            (def.ident == seg.ident).then_some(*index)
                                        }
                                        _ => todo!(),
                                    }
                                }
                                ItemRef::Fn(index) => {
                                    let item = &items_defs[*index];
                                    match item {
                                        ItemActual::Fn(def) => {
                                            (def.ident == seg.ident).then_some(*index)
                                        }
                                        _ => todo!(),
                                    }
                                }
                                ItemRef::Const(index) => {
                                    let item = &items_defs[*index];
                                    match item {
                                        ItemActual::Const(def) => {
                                            (def.name == seg.ident).then_some(*index)
                                        }
                                        _ => todo!(),
                                    }
                                }
                                ItemRef::Trait(index) => {
                                    let item = &items_defs[*index];
                                    match item {
                                        ItemActual::Trait(def) => {
                                            (def.name == seg.ident).then_some(*index)
                                        }
                                        _ => todo!(),
                                    }
                                }
                                _ => None,
                            })
                        } else {
                            None
                        }
                    }
                    _ => None,
                })
                .unwrap();
            (
                vec![PRELUDE_MODULE_PATH.to_string()],
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

// Update definitions
mod update_definitons {
    use std::mem;

    use syn::{
        FnArg, ImplItemFn, Item, ItemConst, ItemEnum, ItemFn, ItemImpl, ItemStruct, ItemTrait,
        Member, Pat, ReturnType, Type,
    };
    use tracing::debug;

    use crate::{
        tree_structure::look_for_module_in_items, RustImplBlockSimple, RustPathSegment, RustType,
        RustTypeParam, RustTypeParamValue, PRELUDE_MODULE_PATH,
    };

    use super::{ItemActual, ItemRef, StmtsRef};

    // pub fn update_item_definitions(
    //     modules: Vec<make_item_definitions::ModuleData>,
    // ) -> (Vec<ModuleData>, Vec<RustImplBlockSimple>) {
    //     // let global_data_copy = global_data.clone();

    //     let mut global_impl_blocks_simpl = Vec::new();
    //     for module in &modules {
    //         let module_path = module.path.clone();

    //         for (scope_id, item_impl) in &module.scoped_syn_impl_items {
    //             // Temporarily store impl block's type params on global data

    //             let impl_item_target_path = match &*item_impl.self_ty {
    //                 Type::Path(type_path) => type_path
    //                     .path
    //                     .segments
    //                     .iter()
    //                     .map(|s| s.ident.to_string())
    //                     .collect::<Vec<_>>(),
    //                 _ => todo!(),
    //             };

    //             let rust_impl_block_generics = item_impl
    //                 .generics
    //                 .params
    //                 .iter()
    //                 .filter_map(|gen| match gen {
    //                     GenericParam::Lifetime(_) => None,
    //                     GenericParam::Type(type_param) => Some(RustGeneric {
    //                         ident: type_param.ident.to_string(),
    //                         trait_bounds: type_param
    //                             .bounds
    //                             .iter()
    //                             .filter_map(|bound| {
    //                                 // First lookup trait
    //                                 match bound {
    //                                     TypeParamBound::Trait(trait_bound) => {
    //                                         let trait_path = trait_bound
    //                                             .path
    //                                             .segments
    //                                             .iter()
    //                                             .map(|seg| seg.ident.to_string());

    //                                         let (module_path, scope_id, trait_def) = modules
    //                                             .lookup_trait_definition_any_module(
    //                                                 &module_path,
    //                                                 &(!scope_id.is_empty()).then_some(scope_id.clone()),
    //                                                 trait_path,
    //                                             );
    //                                         Some((module_path, scope_id, trait_def.name))
    //                                     }
    //                                     TypeParamBound::Lifetime(_) => None,
    //                                     TypeParamBound::Verbatim(_) => todo!(),
    //                                     _ => todo!(),
    //                                 }
    //                             })
    //                             .collect::<Vec<_>>(),
    //                     }),
    //                     GenericParam::Const(_) => todo!(),
    //                 })
    //                 .collect::<Vec<_>>();

    //             let target_type_param = match &*item_impl.self_ty {
    //                 Type::Path(type_path) => {
    //                     if type_path.path.segments.len() == 1 {
    //                         rust_impl_block_generics
    //                             .iter()
    //                             .find(|generic| {
    //                                 let first_seg =
    //                                     type_path.path.segments.first().unwrap().ident.to_string();
    //                                 generic.ident == first_seg
    //                             })
    //                             .cloned()
    //                     } else {
    //                         None
    //                     }
    //                 }
    //                 // TODO handle other `Type`s properly
    //                 _ => None,
    //             };

    //             let trait_path_and_name = item_impl.trait_.as_ref().map(|(_, trait_, _)| {
    //                 let (module_path, trait_scope_id, trait_def) = modules
    //                     .lookup_trait_definition_any_module(
    //                         &module_path,
    //                         &(!scope_id.is_empty()).then_some(scope_id.clone()),
    //                         trait_.segments.iter().map(|seg| seg.ident.to_string()),
    //                     );
    //                 (module_path, trait_scope_id, trait_def.name)
    //             });

    //             let (target_rust_type, _is_target_type_param) =
    //                 if let Some(target_type_param) = target_type_param {
    //                     (
    //                         RustType::TypeParam(RustTypeParam {
    //                             name: target_type_param.ident.clone(),
    //                             type_: RustTypeParamValue::Unresolved,
    //                         }),
    //                         true,
    //                     )
    //                 } else {
    //                     // Get type of impl target

    //                     // dbg!(&module_path);
    //                     // dbg!(&temp_scope_id);
    //                     // dbg!(&impl_item_target_path);
    //                     let (target_item_module, resolved_scope_id, target_item) = modules
    //                         .lookup_item_definition_any_module_or_scope(
    //                             &module_path,
    //                             &(!scope_id.is_empty()).then_some(scope_id.clone()),
    //                             &impl_item_target_path,
    //                         );

    //                     // TODO get rid of RustType::I32 etc, and just use StructOrEnum for everything
    //                     if target_item_module == [PRELUDE_MODULE_PATH] {
    //                         match &target_item.ident[..] {
    //                             "i32" => (RustType::I32, false),
    //                             other => {
    //                                 // TODO just defaulting to this because we want to get rid of all the special RustType variants anyway
    //                                 (
    //                                     RustType::StructOrEnum(
    //                                         target_item
    //                                             .generics
    //                                             .iter()
    //                                             .map(|g| RustTypeParam {
    //                                                 name: g.clone(),
    //                                                 type_: RustTypeParamValue::Unresolved,
    //                                             })
    //                                             .collect::<Vec<_>>(),
    //                                         target_item_module.clone(),
    //                                         resolved_scope_id,
    //                                         target_item.ident.to_string(),
    //                                     ),
    //                                     false,
    //                                 )
    //                             }
    //                         }
    //                     } else {
    //                         (
    //                             RustType::StructOrEnum(
    //                                 target_item
    //                                     .generics
    //                                     .iter()
    //                                     .map(|g| RustTypeParam {
    //                                         name: g.clone(),
    //                                         type_: RustTypeParamValue::Unresolved,
    //                                     })
    //                                     .collect::<Vec<_>>(),
    //                                 target_item_module.clone(),
    //                                 resolved_scope_id,
    //                                 target_item.ident.to_string(),
    //                             ),
    //                             false,
    //                         )
    //                     }
    //                 };

    //             // global_data.impl_block_target_type.pop();

    //             let rust_items = item_impl
    //                 .items
    //                 .iter()
    //                 .map(|syn_item| {
    //                     let item_name = match syn_item {
    //                         ImplItem::Const(_) => todo!(),
    //                         ImplItem::Fn(impl_item_fn) => impl_item_fn.sig.ident.to_string(),
    //                         ImplItem::Type(_) => todo!(),
    //                         ImplItem::Macro(_) => todo!(),
    //                         ImplItem::Verbatim(_) => todo!(),
    //                         _ => todo!(),
    //                     };

    //                     let rust_impl_item_item = match syn_item {
    //                         ImplItem::Const(_) => todo!(),
    //                         ImplItem::Fn(impl_item_fn) => {
    //                             let impl_block_generics =
    //                                 rust_impl_block_generics.iter().map(|g| g.ident.clone());
    //                             let fn_generics = impl_item_fn
    //                                 .sig
    //                                 .generics
    //                                 .params
    //                                 .iter()
    //                                 .filter_map(|g| match g {
    //                                     GenericParam::Lifetime(_) => None,
    //                                     GenericParam::Type(type_param) => {
    //                                         Some(type_param.ident.to_string())
    //                                     }
    //                                     GenericParam::Const(_) => todo!(),
    //                                 })
    //                                 .collect::<Vec<_>>();
    //                             let combined_generics = impl_block_generics
    //                                 .chain(fn_generics.iter().cloned())
    //                                 .collect::<Vec<_>>();

    //                             let inputs_types = impl_item_fn
    //                                 .sig
    //                                 .inputs
    //                                 .iter()
    //                                 .map(|input| match input {
    //                                     FnArg::Receiver(receiver) => {
    //                                         // TODO need to actually parse the reciever to determine if it is boxed or a &mut so we can properly handle derefs
    //                                         // TODO need to ensure we are clear and consistent with the meaning of `RustType::ParentItem`
    //                                         // let rust_type = if receiver.reference.is_some()
    //                                         //     && receiver.mutability.is_some()
    //                                         // {
    //                                         //     RustType::MutRef(Box::new(RustType::ParentItem))
    //                                         // } else {
    //                                         //     RustType::ParentItem
    //                                         // };
    //                                         // (true, false, "self".to_string(), rust_type)

    //                                         let rust_type = if receiver.reference.is_some()
    //                                             && receiver.mutability.is_some()
    //                                         {
    //                                             RustType::MutRef(Box::new(target_rust_type.clone()))
    //                                         } else {
    //                                             target_rust_type.clone()
    //                                         };
    //                                         // (true, false, "self".to_string(), RustType::ParentItem)
    //                                         (true, false, "self".to_string(), rust_type)
    //                                     }
    //                                     FnArg::Typed(pat_type) => (
    //                                         false,
    //                                         match &*pat_type.pat {
    //                                             Pat::Ident(pat_ident) => pat_ident.mutability.is_some(),
    //                                             _ => todo!(),
    //                                         },
    //                                         match &*pat_type.pat {
    //                                             Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
    //                                             _ => todo!(),
    //                                         },
    //                                         parse_types_for_populate_item_definitions(
    //                                             &pat_type.ty,
    //                                             &combined_generics,
    //                                             &module_path,
    //                                             &(!scope_id.is_empty()).then_some(scope_id.clone()),
    //                                             &modules,
    //                                         ),
    //                                     ),
    //                                 })
    //                                 .collect::<Vec<_>>();

    //                             let return_type = match &impl_item_fn.sig.output {
    //                                 ReturnType::Default => RustType::Unit,
    //                                 ReturnType::Type(_, type_) => {
    //                                     parse_types_for_populate_item_definitions(
    //                                         type_,
    //                                         &combined_generics,
    //                                         &module_path,
    //                                         &(!scope_id.is_empty()).then_some(scope_id.clone()),
    //                                         &modules,
    //                                     )
    //                                 }
    //                             };

    //                             // *scope_count += 1;
    //                             // scope_id.push(*scope_count);
    //                             // populate_impl_blocks_items_and_item_def_fields_stmts(
    //                             //     &impl_item_fn.block.stmts,
    //                             //     module,
    //                             //     global_data_copy,
    //                             //     module_path,
    //                             //     global_impl_blocks_simpl,
    //                             //     scope_id,
    //                             // );
    //                             // scope_id.pop();

    //                             let is_pub = match impl_item_fn.vis {
    //                                 Visibility::Public(_) => true,
    //                                 Visibility::Restricted(_) => todo!(),
    //                                 Visibility::Inherited => false,
    //                             };
    //                             RustImplItemItemNoJs::Fn(
    //                                 {
    //                                     if let Some(input) = impl_item_fn.sig.inputs.first() {
    //                                         match input {
    //                                             FnArg::Receiver(_) => true,
    //                                             FnArg::Typed(_) => false,
    //                                         }
    //                                     } else {
    //                                         false
    //                                     }
    //                                 },
    //                                 FnInfo {
    //                                     ident: item_name.clone(),
    //                                     is_pub,
    //                                     inputs_types,
    //                                     generics: fn_generics,
    //                                     return_type,
    //                                     syn: FnInfoSyn::Impl(impl_item_fn.clone()),
    //                                 },
    //                             )
    //                         }
    //                         ImplItem::Type(_) => todo!(),
    //                         ImplItem::Macro(_) => todo!(),
    //                         ImplItem::Verbatim(_) => todo!(),
    //                         _ => todo!(),
    //                     };
    //                     RustImplItemNoJs {
    //                         ident: item_name.clone(),
    //                         item: rust_impl_item_item,
    //                         // syn_object: syn_item.clone(),
    //                     }
    //                 })
    //                 .collect();

    //             global_impl_blocks_simpl.push(RustImplBlockSimple {
    //                 unique_id: get_item_impl_unique_id(
    //                     &module_path,
    //                     &(!scope_id.is_empty()).then_some(scope_id.clone()),
    //                     item_impl,
    //                 ),
    //                 generics: rust_impl_block_generics,
    //                 trait_: trait_path_and_name,
    //                 target: target_rust_type.clone(),
    //                 rust_items,
    //             });
    //         }

    //         // for item in items {
    //         //     // dbg!("populate_impl_blocks_items");
    //         //     // println!("{}", quote! { #item });
    //         //     populate_impl_blocks_items_and_item_def_fields_individual(
    //         //         &item,
    //         //         module,
    //         //         &global_data_copy,
    //         //         &module_path,
    //         //         &mut global_data.impl_blocks_simpl,
    //         //         &mut scope_id,
    //         //         &mut scope_count,
    //         //     );
    //         // }

    //         // Add unique impl block ids to item_def.impl_block_ids
    //         // NOTE it is ok to do this in the same pass as populate_impl_blocks_items_and_item_def_fields because we are only relying on the traits defined in the impl block signature... NO it won't work because we are literally creating the `RustImplBlockSimple`s in populate_impl_blocks_items_and_item_def_fields so they won't all exist until the end of the loop over modules
    //         // populate_item_def_impl_blocks(&mut global_data);
    //         // update_item_def_block_ids(...
    //     }

    //     // TODO avoid clone
    //     let modules_copy = modules.clone();
    //     let new_modules = modules
    //         .into_iter()
    //         .map(|module| {
    //             debug_span!(
    //                 "extract_data_populate_item_definitions module: {:?}",
    //                 module_path = ?module.path
    //             );
    //             let module_path = module.path.clone();

    //             let updated_various_defs = update_various_def(
    //                 module.various_definitions,
    //                 &module_path,
    //                 &None,
    //                 &modules_copy,
    //             );

    //             let updated_scoped_various_defs = module
    //                 .scoped_various_definitions
    //                 .into_iter()
    //                 .map(|(scope, various_def)| {
    //                     let scope_id = Some(scope.clone());
    //                     (
    //                         scope,
    //                         update_various_def(various_def, &module_path, &scope_id, &modules_copy),
    //                     )
    //                 })
    //                 .collect();

    //             ModuleData {
    //                 name: module.name,
    //                 path: module.path,
    //                 pub_submodules: module.pub_submodules,
    //                 private_submodules: module.private_submodules,
    //                 pub_use_mappings: module.pub_use_mappings,
    //                 private_use_mappings: module.private_use_mappings,
    //                 resolved_mappings: module.resolved_mappings,
    //                 various_definitions: updated_various_defs,
    //                 items: module.items,
    //                 scoped_various_definitions: updated_scoped_various_defs,
    //                 scoped_syn_impl_items: module.scoped_syn_impl_items,
    //             }
    //         })
    //         .collect();

    //     (new_modules, global_impl_blocks_simpl)
    // }

    // This simply coverts the list/Vec of item defs to a list/Vec of item defs with the type fields populated (references to other items in the list/Vec) while presevering the order (because we actually get the index from the input Vec, even though we will use it to point to items in the output Vec).
    // However, because we need to know which scoped items are in scope at any given point, we can't just iterate directly over the Vec<ItemActual>, we need to instead iterate over the ItemRef tree, looking for Items.
    // IMPORTANT However, means we need to be careful to preserve the order of the original Vec<ItemActual>. The best approach it probably to create an "empty" Vec initially, and then directly insert the updated defs at the position according to their index.
    pub fn update_item_definitions2(
        module_items_tree: &[ItemRef],
        mut item_defs_no_types: Vec<ItemActual>,
        current_module: &[String],
        in_scope: bool,
    ) -> Vec<ItemV2> {
        // (Vec<ModuleData>, Vec<RustImplBlockSimple>)

        let mut scoped_items = Vec::new();
        let mut updated_item_defs = Vec::with_capacity(item_defs_no_types.len());
        updated_item_defs.resize_with(item_defs_no_types.len(), || ItemV2::None);

        do_things(
            module_items_tree,
            &mut item_defs_no_types,
            current_module,
            &mut updated_item_defs,
            &mut scoped_items,
            in_scope,
        );

        updated_item_defs
    }

    fn do_things(
        module_items_tree: &[ItemRef],
        item_defs_no_types: &mut [ItemActual],
        current_module: &[String],
        updated_item_defs: &mut [ItemV2],
        // TODO use &ItemRef
        scoped_items: &mut Vec<Vec<ItemRef>>,
        in_scope: bool,
    ) {
        for item_ref in module_items_tree {
            match item_ref {
                ItemRef::StructOrEnum(index) => {
                    let item = mem::replace(&mut item_defs_no_types[*index], ItemActual::None);
                    updated_item_defs[*index] = update_item_def(
                        item,
                        current_module,
                        module_items_tree,
                        item_defs_no_types,
                        scoped_items,
                    );

                    if in_scope {
                        scoped_items.last_mut().unwrap().push(item_ref.clone());
                    }
                }
                ItemRef::Fn(index) => {
                    let item = mem::replace(&mut item_defs_no_types[*index], ItemActual::None);
                    updated_item_defs[*index] = update_item_def(
                        item,
                        current_module,
                        module_items_tree,
                        item_defs_no_types,
                        scoped_items,
                    );

                    if in_scope {
                        scoped_items.last_mut().unwrap().push(item_ref.clone());
                    }

                    let fn_info = match &updated_item_defs[*index] {
                        ItemV2::Fn(fn_info) => fn_info,
                        _ => todo!(),
                    };
                    let fn_body_items = fn_info
                        .stmts
                        .iter()
                        .filter_map(|stmt| {
                            match stmt {
                                StmtsRef::Item(item_ref) => Some(item_ref.clone()),
                                // TODO handle item defs in Exprs
                                // StmtsV1::Local(_) => todo!(),
                                // StmtsV1::Expr(_, _) => todo!(),
                                _ => None,
                            }
                        })
                        .collect::<Vec<_>>();

                    // Item in fn body are scoped so create a new scope
                    scoped_items.push(Vec::new());

                    do_things(
                        &fn_body_items,
                        item_defs_no_types,
                        current_module,
                        updated_item_defs,
                        scoped_items,
                        true,
                    );

                    scoped_items.pop();
                }
                ItemRef::Const(index) => {
                    let item = mem::replace(&mut item_defs_no_types[*index], ItemActual::None);
                    updated_item_defs[*index] = update_item_def(
                        item,
                        current_module,
                        module_items_tree,
                        item_defs_no_types,
                        scoped_items,
                    );

                    if in_scope {
                        scoped_items.last_mut().unwrap().push(item_ref.clone());
                    }
                }
                ItemRef::Trait(index) => {
                    let item = mem::replace(&mut item_defs_no_types[*index], ItemActual::None);
                    updated_item_defs[*index] = update_item_def(
                        item,
                        current_module,
                        module_items_tree,
                        item_defs_no_types,
                        scoped_items,
                    );

                    if in_scope {
                        scoped_items.last_mut().unwrap().push(item_ref.clone());
                    }
                }
                ItemRef::Mod(rust_mod) => {
                    do_things(
                        module_items_tree,
                        item_defs_no_types,
                        &rust_mod.module_path,
                        updated_item_defs,
                        scoped_items,
                        false,
                    );
                }
                // TODO
                ItemRef::Impl(_) => {}
                ItemRef::Use(_) => {}
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum ItemV2 {
        StructOrEnum(ItemDefinition),
        Fn(FnInfo),
        Const(ConstDef),
        Trait(RustTraitDefinition),
        Impl(RustImplBlockSimple),
        None,
    }
    impl ItemV2 {
        pub fn ident(&self) -> &str {
            match self {
                ItemV2::StructOrEnum(def) => &def.ident,
                ItemV2::Fn(def) => &def.ident,
                ItemV2::Const(def) => &def.name,
                ItemV2::Trait(def) => &def.name,
                ItemV2::Impl(_) => panic!(),
                ItemV2::None => panic!(),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct RustUse {
        pub pub_use_mappings: Vec<(String, Vec<String>)>,
        pub private_use_mappings: Vec<(String, Vec<String>)>,
    }

    // #[derive(Debug, Clone)]
    // pub struct RustMod {
    //     pub pub_: bool,
    //     // pub pub_use_mappings: Vec<(String, Vec<String>)>,
    //     // pub private_use_mappings: Vec<(String, Vec<String>)>,
    //     pub items: Vec<ItemActual>,
    // }

    #[derive(Debug, Clone)]
    struct RustImplV1 {
        pub syn: ItemImpl,
        pub items: Vec<ImplItemV1>,
    }
    #[derive(Debug, Clone)]
    enum ImplItemV1 {
        Fn(FnInfo),
        Const(ConstDef),
    }

    fn update_item_def(
        item: ItemActual,
        module_path: &[String],
        item_refs: &[ItemRef],
        items_copy: &[ItemActual],
        scoped_items: &[Vec<ItemRef>],
    ) -> ItemV2 {
        match item {
            ItemActual::StructOrEnum(item_def) => {
                let new_struct_or_enum_info = match item_def.struct_or_enum_info {
                    super::StructOrEnumDefitionInfo::Struct(struct_def_info) => {
                        let fields = if struct_def_info.syn_object.fields.is_empty() {
                            StructFieldInfo::UnitStruct
                        } else if struct_def_info
                            .syn_object
                            .fields
                            .iter()
                            .next()
                            .unwrap()
                            .ident
                            .is_some()
                        {
                            StructFieldInfo::RegularStruct(
                                struct_def_info
                                    .syn_object
                                    .fields
                                    .iter()
                                    .map(|f| {
                                        (
                                            f.ident.as_ref().unwrap().to_string(),
                                            parse_types_for_populate_item_definitions(
                                                &f.ty,
                                                &item_def.generics,
                                                module_path,
                                                item_refs,
                                                items_copy,
                                                scoped_items,
                                            ),
                                        )
                                    })
                                    .collect::<Vec<_>>(),
                            )
                        } else {
                            StructFieldInfo::TupleStruct(
                                struct_def_info
                                    .syn_object
                                    .fields
                                    .iter()
                                    .map(|f| {
                                        parse_types_for_populate_item_definitions(
                                            &f.ty,
                                            &item_def.generics,
                                            module_path,
                                            item_refs,
                                            items_copy,
                                            scoped_items,
                                        )
                                    })
                                    .collect::<Vec<_>>(),
                            )
                        };
                        StructOrEnumDefitionInfo::Struct(StructDefinitionInfo {
                            fields,
                            syn_object: struct_def_info.syn_object,
                        })
                    }
                    super::StructOrEnumDefitionInfo::Enum(enum_def_info) => {
                        let members_for_scope = enum_def_info
                            .syn_object
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
                                            &item_def.generics,
                                            module_path,
                                            item_refs,
                                            items_copy,
                                            scoped_items,
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
                        StructOrEnumDefitionInfo::Enum(EnumDefinitionInfo {
                            members: members_for_scope,
                            syn_object: enum_def_info.syn_object,
                        })
                    }
                };
                ItemV2::StructOrEnum(ItemDefinition {
                    ident: item_def.ident,
                    is_copy: item_def.is_copy,
                    is_pub: item_def.is_pub,
                    generics: item_def.generics,
                    struct_or_enum_info: new_struct_or_enum_info,
                    impl_block_ids: item_def.impl_block_ids,
                })
            }
            ItemActual::Fn(fn_info) => {
                // let item_fn = match &fn_info.syn {
                //     make_item_definitions::FnInfoSyn::Standalone(item_fn) => item_fn,
                //     make_item_definitions::FnInfoSyn::Impl(_) => todo!(),
                // };

                let inputs_types = fn_info
                    .signature
                    .inputs
                    .iter()
                    .map(|input| match input {
                        FnArg::Receiver(_) => {
                            // standalone functions cannot have self/receiver inputs
                            panic!();
                        }
                        FnArg::Typed(pat_type) => (
                            false,
                            match &*pat_type.pat {
                                Pat::Ident(pat_ident) => pat_ident.mutability.is_some(),
                                _ => todo!(),
                            },
                            match &*pat_type.pat {
                                Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
                                _ => todo!(),
                            },
                            parse_types_for_populate_item_definitions(
                                &pat_type.ty,
                                &fn_info.generics,
                                module_path,
                                item_refs,
                                items_copy,
                                scoped_items,
                            ),
                        ),
                    })
                    .collect::<Vec<_>>();

                let return_type = match &fn_info.signature.output {
                    ReturnType::Default => RustType::Unit,
                    ReturnType::Type(_, type_) => parse_types_for_populate_item_definitions(
                        type_,
                        &fn_info.generics,
                        module_path,
                        item_refs,
                        items_copy,
                        scoped_items,
                    ),
                };

                ItemV2::Fn(FnInfo {
                    ident: fn_info.ident,
                    is_pub: fn_info.is_pub,
                    inputs_types,
                    generics: fn_info.generics,
                    return_type,
                    stmts: fn_info.stmts,
                    syn: fn_info.syn,
                })
            }
            ItemActual::Const(const_def) => {
                let rust_type = parse_types_for_populate_item_definitions(
                    &const_def.syn_object.ty,
                    &Vec::new(),
                    module_path,
                    item_refs,
                    items_copy,
                    scoped_items,
                );
                ItemV2::Const(ConstDef {
                    name: const_def.name,
                    is_pub: const_def.is_pub,
                    type_: rust_type,
                    syn_object: const_def.syn_object,
                })
            }
            ItemActual::Trait(trait_def) => {
                // Currently trait defs don't store any info other than the name, so we don't need to do anything
                ItemV2::Trait(RustTraitDefinition {
                    name: trait_def.name,
                    is_pub: trait_def.is_pub,
                    syn: trait_def.syn,
                })
            }
            ItemActual::None => panic!(),
        }
    }

    #[allow(clippy::enum_variant_names)]
    #[derive(Debug, Clone)]
    pub enum StructFieldInfo {
        UnitStruct,
        TupleStruct(Vec<RustType>),
        /// (name, type)
        RegularStruct(Vec<(String, RustType)>),
    }

    #[derive(Debug, Clone)]
    pub struct StructDefinitionInfo {
        pub fields: StructFieldInfo,
        pub syn_object: ItemStruct,
    }

    #[derive(Debug, Clone)]
    pub enum EnumVariantInputsInfo {
        Named {
            ident: String,
            input_type: RustType,
        },
        /// (input type)
        Unnamed(RustType),
    }

    #[derive(Debug, Clone)]
    pub struct EnumVariantInfo {
        pub ident: String,
        pub inputs: Vec<EnumVariantInputsInfo>,
    }

    #[derive(Debug, Clone)]
    pub struct EnumDefinitionInfo {
        pub members: Vec<EnumVariantInfo>,
        pub syn_object: ItemEnum,
    }

    #[derive(Debug, Clone)]
    pub enum StructOrEnumDefitionInfo {
        Struct(StructDefinitionInfo),
        Enum(EnumDefinitionInfo),
    }

    /// Similar to StructOrEnum which gets used in RustType, but is for storing info about the actual item definition, rather than instances of, so eg we don't need to be able to store resolved generics. Minor differences but making distinct type helps with reasoning about the different use cases.
    /// Just structs and enums or should we include functions?
    #[derive(Debug, Clone)]
    pub struct ItemDefinition {
        pub ident: String,
        // NOTE we don't need to store the module path because module level `ItemDefinition`s are stored within modules so we will already know the module path
        // module_path: Option<Vec<String>>,
        pub is_copy: bool,
        pub is_pub: bool,
        // /// Fields and enum variants. Methods etc are stored in impl blocks?
        // members: Vec<StructFieldInfo>,
        // members: Vec<ImplItem>,
        // TODO do we need to know eg bounds for each generic?
        pub generics: Vec<String>,
        // syn_object: StructOrEnumSynObject,
        pub struct_or_enum_info: StructOrEnumDefitionInfo,
        // impl_blocks: Vec<ItemDefintionImpls>,
        /// These are uuids/references to all the impl blocks whose target match this struct/enum
        /// (unique impl id)
        pub impl_block_ids: Vec<String>,
    }
    impl ItemDefinition {
        pub fn get_type(&self, field_member: &Member) -> RustType {
            match &self.struct_or_enum_info {
                StructOrEnumDefitionInfo::Struct(struct_def_info) => {
                    match &struct_def_info.fields {
                        StructFieldInfo::UnitStruct => todo!(),
                        StructFieldInfo::TupleStruct(_) => todo!(),
                        StructFieldInfo::RegularStruct(fields2) => fields2
                            .iter()
                            .find_map(|(field_name, field_type)| {
                                let field_member_name = match field_member {
                                    Member::Named(ident) => ident.to_string(),
                                    Member::Unnamed(_) => todo!(),
                                };
                                (field_name == &field_member_name).then_some(field_type)
                            })
                            .unwrap()
                            .clone(),
                    }
                }
                StructOrEnumDefitionInfo::Enum(_) => todo!(),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct RustTraitDefinition {
        pub name: String,
        pub is_pub: bool,
        // impl_items:
        pub syn: ItemTrait,
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
        // /// Update generics based on types of args
        // ///
        // /// For all the generics of the struct/enum...
        // /// ...for enum check if any of the arguments to any of the variants are the generic type...
        // /// ...(if) we found a generic so now we need to find the type of the argument being passed and we will have the full `MyEnum<FoundGeneric>` type
        // fn attempt_to_resolve_generics(
        //     &self,
        //     field_or_variant_name: &String,
        //     args: &Vec<(JsExpr, RustType)>,
        // ) -> Vec<RustTypeParam> {
        //     let mut possibly_resolved_generics = Vec::new();
        //     for generic in &self.generics {
        //         match &self.struct_or_enum_info {
        //             StructOrEnumDefitionInfo::Struct(_) => todo!(),
        //             StructOrEnumDefitionInfo::Enum(enum_def_info) => {
        //                 let item_enum = &enum_def_info.syn_object;
        //                 // ...for enum check if any of the arguments to any of the variants are the generic type...
        //                 for v in &item_enum.variants {
        //                     if v.ident == field_or_variant_name {
        //                         match &v.fields {
        //                             Fields::Named(fields_named) => {
        //                                 for (i, field) in fields_named.named.iter().enumerate() {
        //                                     match &field.ty {
        //                                         Type::Path(type_path) => {
        //                                             if type_path.path.segments.first().unwrap().ident
        //                                                 == generic
        //                                             {
        //                                                 // ...we found a generic so now we need to find the type of the argument being passed and we will have the full `MyEnum<FoundGeneric>` type
        //                                                 let (_js_expr, rust_type) = args[i].clone();
        //                                                 possibly_resolved_generics.push(
        //                                                     RustTypeParam {
        //                                                         name: generic.clone(),
        //                                                         type_: RustTypeParamValue::RustType(
        //                                                             Box::new(rust_type),
        //                                                         ),
        //                                                     },
        //                                                 );
        //                                                 continue;
        //                                             }
        //                                         }
        //                                         Type::Verbatim(_) => todo!(),
        //                                         _ => todo!(),
        //                                     }
        //                                 }
        //                             }
        //                             Fields::Unnamed(fields_unnamed) => {
        //                                 for (i, field) in fields_unnamed.unnamed.iter().enumerate() {
        //                                     match &field.ty {
        //                                         Type::Path(type_path) => {
        //                                             if type_path.path.segments.first().unwrap().ident
        //                                                 == generic
        //                                             {
        //                                                 // ...we found a generic so now we need to find the type of the argument being passed and we will have the full `MyEnum<FoundGeneric>` type
        //                                                 let (_js_expr, rust_type) = args[i].clone();
        //                                                 possibly_resolved_generics.push(
        //                                                     RustTypeParam {
        //                                                         name: generic.clone(),
        //                                                         type_: RustTypeParamValue::RustType(
        //                                                             Box::new(rust_type),
        //                                                         ),
        //                                                     },
        //                                                 );
        //                                                 continue;
        //                                             }
        //                                         }
        //                                         Type::Verbatim(_) => todo!(),
        //                                         _ => todo!(),
        //                                     }
        //                                 }
        //                             }
        //                             Fields::Unit => todo!(),
        //                         }
        //                     }
        //                 }
        //             }
        //         }
        //         possibly_resolved_generics.push(RustTypeParam {
        //             name: generic.clone(),
        //             type_: RustTypeParamValue::Unresolved,
        //         });
        //     }
        //     possibly_resolved_generics
        // }
    }

    #[derive(Debug, Clone)]
    pub struct ModuleData {
        pub name: String,
        // parent_name: Option<String>,
        /// NOTE the path includes the name of the module, eg the path to the crate module is ["crate"] not [].
        pub path: Vec<String>,
        // pub_definitions: Vec<String>,
        // private_definitions: Vec<String>,
        pub pub_submodules: Vec<String>,
        pub private_submodules: Vec<String>,
        /// (snake case item name, snake case use path)
        pub pub_use_mappings: Vec<(String, Vec<String>)>,
        pub private_use_mappings: Vec<(String, Vec<String>)>,
        /// Same format as use mapping but has absolute module path
        /// (snake case item name, snake case absolute module path)
        pub resolved_mappings: Vec<(String, Vec<String>)>,
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
        // // fn_info: Vec<(String, Vec<String>)>,
        // pub fn_info: Vec<FnInfo>,
        // pub item_definitons: Vec<ItemDefinition>,
        // /// (name, type, syn const)
        // pub consts: Vec<ConstDef>,
        // pub trait_definitons: Vec<RustTraitDefinition>,
        pub various_definitions: VariousDefintions,

        // We need this for extract_data_populate_item_definitions which happens after the modules ModuleData has been created by extract_data, but is populating the `ItemDefiitions` etc, and needs access to the original items in the module for this
        pub items: Vec<Item>,

        // (scope number, definitions)
        // scope number is eg [3,4,2] where this is the 3rd scope that appears within the module (not nested inside another scope, eg if the first 3 items are fns, this would be the body block of the 3rd fn, regardless of how many nested scoped there are in the first two fns), the 4th scope within that scope (same rules as before), and then finally the second scope within that scope
        // scoped_various_definitions: Vec<(Vec<usize>, VariousDefintions, Vec<RustImplBlockSimple>)>,
        pub scoped_various_definitions: Vec<(Vec<usize>, VariousDefintions)>,
        pub scoped_syn_impl_items: Vec<(Vec<usize>, ItemImpl)>,
    }
    impl ModuleData {
        pub fn item_defined_in_module(&self, use_private: bool, item: &str) -> bool {
            self.various_definitions
                .item_definitons
                .iter()
                .filter_map(|item_def| (use_private || item_def.is_pub).then_some(&item_def.ident))
                .chain(
                    self.various_definitions
                        .fn_info
                        .iter()
                        .filter_map(|fn_info| {
                            (use_private || fn_info.is_pub).then_some(&fn_info.ident)
                        }),
                )
                .chain(
                    self.various_definitions
                        .trait_definitons
                        .iter()
                        .filter_map(|trait_def| {
                            (use_private || trait_def.is_pub).then_some(&trait_def.name)
                        }),
                )
                .chain(
                    self.various_definitions.consts.iter().filter_map(|const_| {
                        (use_private || const_.is_pub).then_some(&const_.name)
                    }),
                )
                .any(|name| name == item)
        }
        pub fn path_starts_with_sub_module(&self, use_private: bool, item: &str) -> bool {
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

    #[allow(dead_code)]
    trait GetModule {
        fn get_mut(&mut self, module_path: &[String]) -> &mut ModuleData;
    }
    impl GetModule for Vec<ModuleData> {
        fn get_mut(&mut self, module_path: &[String]) -> &mut ModuleData {
            self.iter_mut().find(|m| m.path == module_path).unwrap()
        }
    }

    #[derive(Debug, Clone, Default)]
    pub struct VariousDefintions {
        pub fn_info: Vec<FnInfo>,
        pub item_definitons: Vec<ItemDefinition>,
        pub consts: Vec<ConstDef>,
        pub trait_definitons: Vec<RustTraitDefinition>,
    }

    #[derive(Debug, Clone)]
    pub struct ConstDef {
        pub name: String,
        pub is_pub: bool,
        pub type_: RustType,
        pub syn_object: ItemConst,
    }

    /// Not just for methods, can also be an enum variant with no inputs
    #[derive(Debug, Clone)]
    pub struct FnInfo {
        // TODO No point storing all the info like inputs and return types separately, as these need to be stored on RustType::Fn anyway for eg closures where we won't be storing a fn info?? Keep both for now and revisit later. Note fns idents can just appear in the code and be called whereas a closure will be a var which already has a type.
        pub ident: String,
        pub is_pub: bool,
        /// Does this include receiver/self types? NO in handle_item_fn we are filtering out any self type. Could just store it as RustType::Self, but seems pointless if we don't actually need it for anything. NO again, updated to include self inputs because we need them.
        /// TODO probably don't actually need `is_self`
        /// (is_self, is_mut, name, type)
        pub inputs_types: Vec<(bool, bool, String, RustType)>,
        pub generics: Vec<String>,
        // NO! for methods we want to store the actual fn type. fns can be assigned to vars, and we want to be able to pass the Path part of the fn, and *then* call it and determine the return type
        pub return_type: RustType,
        pub stmts: Vec<StmtsRef>,
        // /// type of fn eg Fn(i32) -> ()
        // rust_type: RustType,
        // TODO optionally add enum for Field, AssociatedFn, Method, etc
        pub syn: FnInfoSyn,
    }

    #[derive(Debug, Clone)]
    pub enum FnInfoSyn {
        Standalone(ItemFn),
        Impl(ImplItemFn),
    }

    impl FnInfo {
        pub fn attempt_to_resolve_type_params_using_arg_types(
            &self,
            args: &[RustType],
        ) -> Vec<RustTypeParam> {
            self.generics
                .iter()
                .map(|g| {
                    let matched_arg_rust_type = self.inputs_types.iter().enumerate().find_map(
                        |(i, (_is_self, _is_mut, _name, input_type))| {
                            match input_type {
                                RustType::TypeParam(type_param) if g == &type_param.name => {
                                    Some(args[i].clone())
                                }
                                // TODO what about types that *contain* a type param eg `foo: Option<T>`
                                _ => None,
                            }
                        },
                    );

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

    /// Similar to parse_fn_input_or_field but for the extract_data_populate_item_definitions() pass before parsing, so only dealing with top level items, so don't need to check for scoped item definitions, also given we are popualting `.item_definitions()` etc, we need to avoid using these. TODO IMPORTANT no I believe we are also dealing with scoped items in `extract_data_populate_item_definitions()`
    ///
    /// Suitable for parsing: fn input types, fn return type, struct fields, enum variants with args
    ///
    /// NOTE global data is required by get_path_without_namespacing which only uses pub_definitions etc, not `ItemDefintion`s
    ///
    /// IMPORTANT NOTE this fn is never used in the first pass where item definitions are being recorded, only in the second pass where info about dependant types is being add, so we can safely lookup Path -> ItemDefinition here
    #[allow(unused_variables)]
    fn parse_types_for_populate_item_definitions(
        type_: &Type,
        // NOTE this will simply be empty for items that can't be generic, ie consts, or can but simply don't have any
        root_parent_item_definition_generics: &[String],
        // TODO should just store the current module in GlobalData to save having to pass this around everywhere
        current_module: &[String],
        // global_data: &make_item_definitions::GlobalData,
        item_refs: &[ItemRef],
        item_defs: &[ItemActual],
        scoped_items: &[Vec<ItemRef>],
    ) -> RustType {
        todo!()
    }

    // #[allow(dead_code)]
    // #[derive(Debug, Clone, PartialEq, Eq)]
    // enum RustTypeImplTrait {
    //     SimpleTrait(String),
    //     /// (return type)
    //     Fn(RustType),
    // }

    #[allow(clippy::too_many_arguments)]
    /// -> (module path, item path, is scoped item, item index)
    pub fn resolve_path(
        // look_for_scoped_vars: bool,
        // TODO can we combine this with `look_for_scoped_vars`?
        _look_for_scoped_items: bool,
        use_private_items: bool,
        mut segs: Vec<RustPathSegment>,
        module_items: &[ItemRef],
        // TODO replace GlobalData with `.modules` and `.scopes` to making setting up test cases easier
        // global_data: &GlobalData,
        items_defs: &[ItemActual],
        // scopes: &[GlobalDataScope],
        current_mod: &[String],
        // Only used to determine if current module is the original module
        orig_mod: &[String],
        scoped_items: &Vec<Vec<ItemRef>>,
    ) -> (Vec<String>, Vec<RustPathSegment>, bool, Option<usize>) {
        debug!(segs = ?segs, "get_path_without_namespacing");

        // TODO I don't think we need to pass in the module `ModuleData` if we are already passing the `current_module` module path we can just use that to look it up each time, which might be less efficient since we shouldn't need to lookup the module if we haven't changed modules (though I think we are pretty much always changing modules except for use statements?), but we definitely don't want to pass in both. Maybe only pass in `module: &ModuleData` and not `current_module`
        // assert!(current_module == &module.path);

        let module = look_for_module_in_items(module_items, items_defs, current_mod).unwrap();

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
        // let scoped_use_mapping = scopes
        //     .iter()
        //     .rev()
        //     .find_map(|s| s.use_mappings.iter().find(|u| u.0 == segs[0].ident));

        // let mut use_mappings = module.pub_use_mappings.iter();
        // let matched_use_mapping = if use_private_items || is_parent_or_same_module {
        //     use_mappings
        //         .chain(module.private_use_mappings.iter())
        //         .find(|use_mapping| use_mapping.0 == segs[0].ident)
        // } else {
        //     use_mappings.find(|use_mapping| use_mapping.0 == segs[0].ident)
        // };
        // dbg!(matched_use_mapping);
        let matched_use_mapping = module.items.iter().find_map(|item| match item {
            ItemRef::Use(rust_use) => rust_use.use_mapping.iter().find_map(|use_mapping| {
                (use_mapping.0 == segs[0].ident && (use_private || rust_use.pub_))
                    .then_some(use_mapping.clone())
            }),
            _ => None,
        });

        // TODO can module shadow external crate names? In which case we need to look for modules first? I think we do this implicitly based on the order of the if statements below?
        // TODO actually look up external crates in Cargo.toml
        let external_crate_names = ["web_prelude"];
        let path_is_external_crate = external_crate_names.iter().any(|cn| cn == &segs[0].ident);

        // Look for scoped item
        let scoped_item = scoped_items.iter().rev().find_map(|s| {
            s.iter().find_map(|item| match item {
                ItemRef::StructOrEnum(index) => {
                    let item = &items_defs[*index];
                    let def = match item {
                        ItemActual::StructOrEnum(def) => def,
                        _ => todo!(),
                    };
                    (def.ident == segs[0].ident).then_some(*index)
                }
                ItemRef::Fn(index) => {
                    let item = &items_defs[*index];
                    let def = match item {
                        ItemActual::Fn(fn_info) => fn_info,
                        _ => todo!(),
                    };
                    (def.ident == segs[0].ident).then_some(*index)
                }
                ItemRef::Const(index) => {
                    let item = &items_defs[*index];
                    let def = match item {
                        ItemActual::Const(def) => def,
                        _ => todo!(),
                    };
                    (def.name == segs[0].ident).then_some(*index)
                }
                ItemRef::Trait(index) => {
                    let item = &items_defs[*index];
                    let def = match item {
                        ItemActual::Trait(def) => def,
                        _ => todo!(),
                    };
                    (def.name == segs[0].ident).then_some(*index)
                }
                ItemRef::Mod(_) => None,
                ItemRef::Impl(_) => None,
                ItemRef::Use(_) => None,
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
                module_items,
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
                module_items,
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
                module_items,
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
                module_items,
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
                module_items,
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
                module_items,
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
            {
                // TODO IMPORTANT we aren't meant to be handling these in get_path, they should be handled in the item def passes, not the JS parsing. add a panic!() here. NO not true, we will have i32, String, etc in closure defs, type def for var assignments, etc.
                // TODO properly encode "prelude_special_case" in a type rather than a String
                let item_index = module_items
                    .iter()
                    .find_map(|item_ref| match item_ref {
                        ItemRef::Mod(rust_mod) => {
                            if rust_mod.module_path == [PRELUDE_MODULE_PATH] {
                                rust_mod.items.iter().find_map(|item_ref| match item_ref {
                                    ItemRef::StructOrEnum(index) => {
                                        let item = &items_defs[*index];
                                        match item {
                                            ItemActual::StructOrEnum(def) => {
                                                (def.ident == seg.ident).then_some(*index)
                                            }
                                            _ => todo!(),
                                        }
                                    }
                                    ItemRef::Fn(index) => {
                                        let item = &items_defs[*index];
                                        match item {
                                            ItemActual::Fn(def) => {
                                                (def.ident == seg.ident).then_some(*index)
                                            }
                                            _ => todo!(),
                                        }
                                    }
                                    ItemRef::Const(index) => {
                                        let item = &items_defs[*index];
                                        match item {
                                            ItemActual::Const(def) => {
                                                (def.name == seg.ident).then_some(*index)
                                            }
                                            _ => todo!(),
                                        }
                                    }
                                    ItemRef::Trait(index) => {
                                        let item = &items_defs[*index];
                                        match item {
                                            ItemActual::Trait(def) => {
                                                (def.name == seg.ident).then_some(*index)
                                            }
                                            _ => todo!(),
                                        }
                                    }
                                    _ => None,
                                })
                            } else {
                                None
                            }
                        }
                        _ => None,
                    })
                    .unwrap();
                (
                    vec![PRELUDE_MODULE_PATH.to_string()],
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
}
