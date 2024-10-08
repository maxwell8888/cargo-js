use std::rc::Rc;

use tracing::debug_span;

use crate::{
    make_item_definitions::{ItemRef, RustMod, StmtsRef},
    update_item_definitions::{ImplBlockDef, ItemDef, ItemDefRc, RustType, StructEnumDef},
    RUST_PRELUDE_MODULE_PATH,
};

fn get_traits_implemented_for_item(
    item_impls: &[(usize, ImplBlockDef)],
    item_module_path: &[String],
    item_name: &str,
    // TODO change to Vec<usize>
) -> Vec<(Vec<String>, String, usize)> {
    // Does class implement the trait bounds of the impl block

    // TODO this code is duplicated from elsewhere
    // Each time we find new traits we need to again look for matching traits, and repeat this until we don't find any new traits
    let mut found_traits_count = 0;
    // (trait module path, trait name, trait index)
    let mut found_traits: Vec<(Vec<String>, String, usize)> = Vec::new();
    loop {
        found_traits.clear();

        for (_index, item_impl) in item_impls {
            // TODO this needs extending to handle matching any target type, rather than just user structs
            match &item_impl.target {
                RustType::TypeParam(rust_type_param) => {
                    // If the target is a type param then we must be implementing a trait
                    let rust_trait = item_impl.trait_.clone().unwrap();

                    // Get bounds on type param
                    let type_param_bounds = &item_impl
                        .generics
                        .iter()
                        .find(|generic| generic.ident == rust_type_param.name)
                        .unwrap()
                        .trait_bounds;

                    // Does our struct impl all of these traits?
                    let struct_impls_all_bounds =
                        type_param_bounds.iter().all(|type_param_bound| {
                            found_traits
                                .iter()
                                .map(|(_module_path, _name, index)| *index)
                                .collect::<Vec<_>>()
                                .contains(type_param_bound)
                        });
                    if struct_impls_all_bounds {
                        found_traits.push(rust_trait);
                    }
                }
                RustType::StructOrEnum(_, _, _, _) | RustType::I32 => {
                    if let Some(impl_trait) = &item_impl.trait_ {
                        let types_match = match &item_impl.target {
                            RustType::StructOrEnum(_type_params, module_path, name, _index) => {
                                module_path == item_module_path && name == item_name
                            }
                            RustType::I32 => {
                                item_module_path == [RUST_PRELUDE_MODULE_PATH] && item_name == "i32"
                            }
                            RustType::Option(_) => {
                                item_module_path == [RUST_PRELUDE_MODULE_PATH]
                                    && item_name == "Option"
                            }
                            _ => {
                                dbg!(&item_module_path);
                                dbg!(&item_name);
                                todo!()
                            }
                        };

                        if types_match {
                            found_traits.push(impl_trait.clone());
                        }
                    }
                }
                RustType::MutRef(_) => todo!(),
                RustType::Ref(_) => todo!(),
                _ => todo!(),
            }
        }

        if found_traits_count == found_traits.len() {
            break;
        } else {
            found_traits_count = found_traits.len();
        }
    }

    found_traits
}

fn extract_impl_blocks(
    item_refs: &[ItemRef],
    item_defs: &[ItemDef],
    impl_blocks: &mut Vec<(usize, ImplBlockDef)>,
) {
    for item_ref in item_refs {
        match item_ref {
            ItemRef::Impl(index) => {
                let impl_def = match item_defs[*index].clone() {
                    ItemDef::Impl(impl_def) => impl_def,
                    _ => todo!(),
                };
                impl_blocks.push((*index, impl_def))
            }
            ItemRef::Mod(rust_mod) => {
                extract_impl_blocks(&rust_mod.items, item_defs, impl_blocks);
            }
            ItemRef::Fn(index) => match item_defs[*index].clone() {
                ItemDef::Fn(fn_info) => {
                    let item_refs = fn_info
                        .stmts
                        .iter()
                        .filter_map(|stmt_ref| match stmt_ref {
                            StmtsRef::Item(item_ref) => Some(item_ref.clone()),
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    extract_impl_blocks(&item_refs, item_defs, impl_blocks)
                }
                _ => todo!(),
            },
            _ => {}
        }
    }
}

/// Populates `item_def.impl_blocks: Vec<String>` with ids of impl blocks
pub fn populate_item_def_impl_blocks(
    crates: &[RustMod],
    item_defs: &mut [ItemDef],
    // impl_blocks: &[(usize, RustImplBlockSimple)],
) {
    // IMPORTANT TODO all this needs improving, especially to ensure we are only trying to match scoped impls that can actually reach the item. Need unit tests.

    let span = debug_span!("update_classes");
    let _guard = span.enter();

    let item_refs = crates
        .iter()
        .flat_map(|rust_mod| rust_mod.items.clone())
        .collect::<Vec<_>>();
    let mut impl_blocks = Vec::new();
    extract_impl_blocks(&item_refs, item_defs, &mut impl_blocks);

    // dbg!(impl_blocks
    //     .iter()
    //     .map(|impl_block| &impl_block.1.trait_)
    //     .collect::<Vec<_>>());
    // dbg!(impl_blocks
    //     .iter()
    //     .map(|(index, block)| (index, &block.syn.self_ty))
    //     .collect::<Vec<_>>());

    // Recursively update all structs/enums

    // TODO assuming first recursion should be finding only modules so can just past empty module name to start
    for rust_mod in crates {
        update_all_structs_enums(
            &rust_mod.items,
            item_defs,
            rust_mod.module_path.clone(),
            &impl_blocks,
        );
    }

    // let prelude_module = global_data
    //     .modules
    //     .iter_mut()
    //     .find(|m| m.path == [PRELUDE_MODULE_PATH])
    //     .unwrap();
    // // dbg!(&prelude_module);
    // for item_def in prelude_module
    //     .item_definitons
    //     .iter_mut()
    //     .filter(|item_def| item_def.ident != "Box")
    // {
    //     update_item_def_block_ids(item_def, &None, &vec!["donotuse".to_string()], &impl_blocks);
    // }
}

fn update_all_structs_enums(
    item_refs: &[ItemRef],
    item_defs: &mut [ItemDef],
    current_module: Vec<String>,
    impl_blocks: &[(usize, ImplBlockDef)],
) {
    for item_ref in item_refs {
        match item_ref {
            ItemRef::StructOrEnum(index) => {
                update_item_def_block_ids(
                    *index,
                    item_defs,
                    // &item_def_scope_id,
                    &current_module,
                    impl_blocks,
                )
            }
            ItemRef::Mod(rust_mod) => {
                update_all_structs_enums(
                    &rust_mod.items,
                    item_defs,
                    rust_mod.module_path.clone(),
                    impl_blocks,
                );
            }
            ItemRef::Fn(index) => match item_defs[*index].clone() {
                ItemDef::Fn(fn_info) => {
                    let item_refs = fn_info
                        .clone()
                        .stmts
                        .into_iter()
                        .filter_map(|stmt_ref| match stmt_ref {
                            StmtsRef::Item(item_ref) => Some(item_ref),
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    update_all_structs_enums(
                        &item_refs,
                        item_defs,
                        current_module.clone(),
                        impl_blocks,
                    )
                }
                _ => todo!(),
            },
            _ => {}
        }
    }
}

fn update_item_def_block_ids(
    item_index: usize,
    item_defs: &mut [ItemDef],
    // item_def_scope_id: &Option<Vec<usize>>,
    module_path: &[String],
    // global_data: &GlobalData,
    impl_blocks: &[(usize, ImplBlockDef)],
) {
    let actual = &item_defs[item_index];
    let item_def_ident = match actual {
        ItemDef::StructEnum(def) => def.ident.clone(),
        _ => todo!(),
    };
    let traits_impld_for_class =
        get_traits_implemented_for_item(impl_blocks, module_path, &item_def_ident);

    let mut trait_with_defaults = Vec::new();
    for (module_path, name, index) in &traits_impld_for_class {
        let trait_def = match &item_defs[*index] {
            ItemDef::Trait(trait_def) => trait_def,
            _ => todo!(),
        };
        // Check if trait has any default impls, and if so record it's id on the struct/enum
        if !trait_def.default_impls.is_empty() {
            trait_with_defaults.push(*index);
        }
    }

    let actual = item_defs.get_mut(item_index).unwrap();
    let item_def = match actual {
        ItemDef::StructEnum(def) => def,
        _ => todo!(),
    };

    item_def.traits = trait_with_defaults;

    // for impl_block in impl_blocks.iter().chain(scoped_impl_blocks.clone()) {
    for (index, impl_block) in impl_blocks {
        // NOTE we differentiate between concrete and type param targets because for (non-generic TODO) concrete types we only have to match on item name/id, whereas for type params we have to check if the item implements all the type bounds
        match &impl_block.target {
            // Concrete type target
            RustType::StructOrEnum(
                _,
                _impl_target_module_path,
                _impl_target_name,
                impl_tartget_index,
            ) => {
                // The purpose of storing this info on the item_def is so that after the syn -> JS parsing parsing has happened and we have a parsed impl block and items, we can use this info to lookup this parsed impl block and copy it's methods/fields to the class.
                if item_index == *impl_tartget_index {
                    item_def.impl_block_ids.push(*index);
                }
                // if &item_def.ident == impl_target_name && module_path == impl_target_module_path {
                //     // item_def
                //     //     .impl_blocks
                //     //     .push(ItemDefintionImpls::ConcreteImpl(impl_block.items.clone()));

                //     item_def.impl_block_ids.push(*index);
                // }
            }
            RustType::I32 => {
                if item_def.ident == "i32" && module_path == [RUST_PRELUDE_MODULE_PATH] {
                    item_def.impl_block_ids.push(*index);
                }
            }
            // For a generic impl like `impl<T> Foo for T {}`, remember this will add methods to matching items in *all* scopes, both parent and children, the only restriction is the the `impl`d trait must be in accessible/in scope *when the method is called*
            RustType::TypeParam(rust_type_param) => {
                // Get bounds on type param
                let type_param_bounds = &impl_block
                    .generics
                    .iter()
                    .find(|generic| generic.ident == rust_type_param.name)
                    .unwrap()
                    .trait_bounds;

                // Does our struct impl all of these traits?
                let struct_impls_all_bounds = type_param_bounds.iter().all(|type_param_bound| {
                    traits_impld_for_class
                        .iter()
                        .map(|(_module_path, _name, index)| *index)
                        .collect::<Vec<_>>()
                        .contains(type_param_bound)
                });

                // TODO we might be adding this impl to items that meet the trait bound, but methods from the impl'd trait are not acutally called. There doesn't seem to be any easy, direct way to check this, the best approach is possibly for each method stored on the `ItemDefinition` to have a flag which get's set to true when the method is called/looked up, and otherwise remains false and is not used/output when the item/class is written to JS.

                if struct_impls_all_bounds {
                    item_def.impl_block_ids.push(*index);
                }
            }
            // IMPORTANT NOTE I did have a todo! here but it would without fail cause rust-analyzer to crash when I moved my cursor there, and take down vscode with it
            _ => {}
        }
    }
}
