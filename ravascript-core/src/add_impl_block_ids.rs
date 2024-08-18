use tracing::debug_span;

use crate::{
    make_item_definitions::{ItemRef, StmtsRef},
    update_item_definitions::{ItemDefinition, ItemV2, RustImplBlockSimple, RustType},
    PRELUDE_MODULE_PATH,
};

fn get_traits_implemented_for_item(
    item_impls: &[(usize, RustImplBlockSimple)],
    item_module_path: &[String],
    item_name: &str,
    // TODO change to Vec<usize>
) -> Vec<(Vec<String>, String, usize)> {
    // Does class implement the trait bounds of the impl block

    // TODO this code is duplicated from elsewhere
    // Each time we find new traits we need to again look for matching traits, and repeat this until we don't find any new traits
    let mut found_traits_count = 0;
    // (trait module path (None for scoped), trait name)
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
                                item_module_path == [PRELUDE_MODULE_PATH] && item_name == "i32"
                            }
                            RustType::Option(_) => {
                                item_module_path == [PRELUDE_MODULE_PATH] && item_name == "Option"
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
    item_defs: &[ItemV2],
    impl_blocks: &mut Vec<(usize, RustImplBlockSimple)>,
) {
    for item_ref in item_refs {
        match item_ref {
            ItemRef::Impl(index) => {
                let impl_def = match item_defs[*index].clone() {
                    ItemV2::Impl(impl_def) => impl_def,
                    _ => todo!(),
                };
                impl_blocks.push((*index, impl_def))
            }
            ItemRef::Mod(rust_mod) => {
                extract_impl_blocks(&rust_mod.items, item_defs, impl_blocks);
            }
            ItemRef::Fn(index) => match item_defs[*index].clone() {
                ItemV2::Fn(fn_info) => {
                    let item_refs = fn_info
                        .clone()
                        .stmts
                        .into_iter()
                        .filter_map(|stmt_ref| match stmt_ref {
                            StmtsRef::Item(item_ref) => Some(item_ref),
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
    item_refs: &[ItemRef],
    item_defs: &mut [ItemV2],
    // impl_blocks: &[(usize, RustImplBlockSimple)],
) {
    // IMPORTANT TODO all this needs improving, especially to ensure we are only trying to match scoped impls that can actually reach the item. Need unit tests.

    let span = debug_span!("update_classes");
    let _guard = span.enter();

    let mut impl_blocks = Vec::new();
    extract_impl_blocks(item_refs, item_defs, &mut impl_blocks);
    // dbg!(impl_blocks
    //     .iter()
    //     .map(|(index, block)| (index, &block.syn.self_ty))
    //     .collect::<Vec<_>>());

    // Recursively update all structs/enums

    // TODO assuming first recursion should be finding only modules so can just past empty module name to start
    update_all_structs_enums(item_refs, item_defs, vec![], &impl_blocks);

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
    item_defs: &mut [ItemV2],
    current_module: Vec<String>,
    impl_blocks: &[(usize, RustImplBlockSimple)],
) {
    for item_ref in item_refs {
        match item_ref {
            ItemRef::StructOrEnum(index) => {
                let actual = item_defs.get_mut(*index).unwrap();
                let item_def = match actual {
                    ItemV2::StructOrEnum(def) => def,
                    _ => todo!(),
                };
                update_item_def_block_ids(
                    *index,
                    item_def,
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
                ItemV2::Fn(fn_info) => {
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
    item_def: &mut ItemDefinition,
    // item_def_scope_id: &Option<Vec<usize>>,
    module_path: &[String],
    // global_data: &GlobalData,
    impl_blocks: &[(usize, RustImplBlockSimple)],
) {
    let traits_impld_for_class =
        get_traits_implemented_for_item(impl_blocks, module_path, &item_def.ident);
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
                if item_def.ident == "i32" && module_path == [PRELUDE_MODULE_PATH] {
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
