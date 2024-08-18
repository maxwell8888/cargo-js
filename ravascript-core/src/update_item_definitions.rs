use syn::{ItemEnum, ItemImpl, ItemStruct, ItemTrait, Member};
use tracing::debug;

use crate::{
    handle_syn::{GlobalData, RustType2, RustTypeImplTrait2, RustTypeParam2, RustTypeParamValue2},
    js_ast::Ident,
    make_item_definitions::{
        look_for_module_in_items, ExprRef, FnInfoSyn, ImplItemExprStmtRefs, ItemActual,
        StructOrEnumDefitionInfo,
    },
};

use syn::{
    FnArg, GenericArgument, GenericParam, ImplItem, Pat, PathArguments, ReturnType, Type,
    TypeParamBound, Visibility,
};

use crate::{
    duplicate_namespacing::Duplicate,
    make_item_definitions::{self},
    RustPathSegment, PRELUDE_MODULE_PATH,
};

use super::{ItemRef, StmtsRef};

// This simply coverts the list/Vec of item defs to a list/Vec of item defs with the type fields populated (references to other items in the list/Vec) while presevering the order (because we actually get the index from the input Vec, even though we will use it to point to items in the output Vec).
// However, because we need to know which scoped items are in scope at any given point, we can't just iterate directly over the Vec<ItemActual>, we need to instead iterate over the ItemRef tree, looking for Items.
// IMPORTANT However, means we need to be careful to preserve the order of the original Vec<ItemActual>. The best approach it probably to create an "empty" Vec initially, and then directly insert the updated defs at the position according to their index.
pub fn update_item_definitions2(
    item_refs: &[ItemRef],
    mut item_defs_no_types: Vec<ItemActual>,
    current_module: &[String],
    in_scope: bool,
    duplicates: Vec<Duplicate>,
) -> Vec<ItemV2> {
    // (Vec<ModuleData>, Vec<RustImplBlockSimple>)

    let mut scoped_items = Vec::new();
    let mut updated_item_defs = Vec::with_capacity(item_defs_no_types.len());
    updated_item_defs.resize_with(item_defs_no_types.len(), || ItemV2::None);

    for item_ref in item_refs {
        update_item_defs_recurisve_individual_item(
            item_ref,
            item_refs,
            &mut item_defs_no_types,
            current_module,
            &mut updated_item_defs,
            &mut scoped_items,
            in_scope,
            &duplicates,
        );
    }

    updated_item_defs
}

fn update_item_defs_recurisve_individual_item(
    item_ref: &ItemRef,
    item_refs_all: &[ItemRef],
    item_defs_no_types: &mut [ItemActual],
    current_module: &[String],
    updated_item_defs: &mut [ItemV2],
    // TODO use &ItemRef
    scoped_items: &mut Vec<Vec<ItemRef>>,
    in_scope: bool,
    duplicates: &[Duplicate],
) {
    match item_ref {
        ItemRef::StructOrEnum(index) => {
            // TODO moving the def means we no longer have a complete list of items to lookup eg Type::Path names in. Solution is to have optional fields on the def and update it in place?
            // let item = mem::replace(&mut item_defs_no_types[*index], ItemActual::None);
            let item = item_defs_no_types[*index].clone();
            updated_item_defs[*index] = update_item_def(
                item,
                current_module,
                item_refs_all,
                item_defs_no_types,
                scoped_items,
                in_scope,
                duplicates,
            );

            if in_scope {
                scoped_items.last_mut().unwrap().push(item_ref.clone());
            }
        }
        ItemRef::Fn(index) => {
            // let item = mem::replace(&mut item_defs_no_types[*index], ItemActual::None);
            let item = item_defs_no_types[*index].clone();
            updated_item_defs[*index] = update_item_def(
                item,
                current_module,
                item_refs_all,
                item_defs_no_types,
                scoped_items,
                in_scope,
                duplicates,
            );

            if in_scope {
                scoped_items.last_mut().unwrap().push(item_ref.clone());
            }

            let fn_stmts = match &updated_item_defs[*index] {
                ItemV2::Fn(fn_info) => fn_info.stmts.clone(),
                _ => todo!(),
            };

            // Item in fn body are scoped so create a new scope
            scoped_items.push(Vec::new());

            update_item_defs_recurisve_stmts(
                &fn_stmts,
                item_refs_all,
                item_defs_no_types,
                current_module,
                updated_item_defs,
                scoped_items,
                true,
                duplicates,
            );

            scoped_items.pop();
        }
        ItemRef::Const(index) => {
            // let item = mem::replace(&mut item_defs_no_types[*index], ItemActual::None);
            let item = item_defs_no_types[*index].clone();
            updated_item_defs[*index] = update_item_def(
                item,
                current_module,
                item_refs_all,
                item_defs_no_types,
                scoped_items,
                in_scope,
                duplicates,
            );

            if in_scope {
                scoped_items.last_mut().unwrap().push(item_ref.clone());
            }
        }
        ItemRef::Trait(index) => {
            // let item = mem::replace(&mut item_defs_no_types[*index], ItemActual::None);
            let item = item_defs_no_types[*index].clone();
            updated_item_defs[*index] = update_item_def(
                item,
                current_module,
                item_refs_all,
                item_defs_no_types,
                scoped_items,
                in_scope,
                duplicates,
            );

            if in_scope {
                scoped_items.last_mut().unwrap().push(item_ref.clone());
            }
        }
        ItemRef::Mod(rust_mod) => {
            for item_ref in &rust_mod.items {
                update_item_defs_recurisve_individual_item(
                    item_ref,
                    item_refs_all,
                    item_defs_no_types,
                    &rust_mod.module_path,
                    updated_item_defs,
                    scoped_items,
                    false,
                    duplicates,
                );
            }
        }
        // TODO
        ItemRef::Impl(index) => {
            let item = item_defs_no_types[*index].clone();
            updated_item_defs[*index] = update_item_def(
                item,
                current_module,
                item_refs_all,
                item_defs_no_types,
                scoped_items,
                in_scope,
                duplicates,
            );

            if in_scope {
                // TODO
                // scoped_items.last_mut().unwrap().push(item_ref.clone());
            }

            let fn_stmts = match &updated_item_defs[*index] {
                ItemV2::Impl(rust_impl_block) => {
                    rust_impl_block
                        .rust_items
                        .iter()
                        .map(|rust_impl_item_no_js| {
                            match &rust_impl_item_no_js.item {
                                RustImplItemItemNoJs::Fn(static_, fn_info) => {
                                    // Item in fn body are scoped so create a new scope
                                    fn_info.stmts.clone()
                                }
                                RustImplItemItemNoJs::Const => todo!(),
                            }
                        })
                        .collect::<Vec<_>>()
                }
                _ => todo!(),
            };
            for stmts in fn_stmts {
                scoped_items.push(Vec::new());

                update_item_defs_recurisve_stmts(
                    &stmts,
                    item_refs_all,
                    item_defs_no_types,
                    current_module,
                    updated_item_defs,
                    scoped_items,
                    true,
                    duplicates,
                );

                scoped_items.pop();
            }
        }
        ItemRef::Use(_) => {}
        // TODO
        ItemRef::Macro => {}
    }
}

fn update_item_defs_recurisve_stmts(
    stmt_refs: &[StmtsRef],
    item_refs_all: &[ItemRef],
    item_defs_no_types: &mut [ItemActual],
    current_module: &[String],
    updated_item_defs: &mut [ItemV2],
    // TODO use &ItemRef
    scoped_items: &mut Vec<Vec<ItemRef>>,
    in_scope: bool,
    duplicates: &[Duplicate],
) {
    // TODO handle all cases
    for stmt_ref in stmt_refs {
        match stmt_ref {
            StmtsRef::Local(_) => {}
            StmtsRef::Item(item_ref) => {
                update_item_defs_recurisve_individual_item(
                    item_ref,
                    item_refs_all,
                    item_defs_no_types,
                    current_module,
                    updated_item_defs,
                    scoped_items,
                    in_scope,
                    duplicates,
                );
            }
            StmtsRef::Expr(expr_ref, _) => {
                update_item_defs_recurisve_individual_expr(
                    expr_ref,
                    item_refs_all,
                    item_defs_no_types,
                    current_module,
                    updated_item_defs,
                    scoped_items,
                    in_scope,
                    duplicates,
                );
            }
            StmtsRef::Macro(_) => {}
        }
    }
}

fn update_item_defs_recurisve_individual_expr(
    expr_ref: &ExprRef,
    item_refs_all: &[ItemRef],
    item_defs_no_types: &mut [ItemActual],
    current_module: &[String],
    updated_item_defs: &mut [ItemV2],
    // TODO use &ItemRef
    scoped_items: &mut Vec<Vec<ItemRef>>,
    in_scope: bool,
    duplicates: &[Duplicate],
) {
    // TODO IMPORTANT handle all cases
    match expr_ref {
        ExprRef::Array(_) => {}
        ExprRef::Assign(_) => {}
        ExprRef::Async(_) => {}
        ExprRef::Await(_) => {}
        ExprRef::Binary(_) => {}
        ExprRef::Block(rust_expr_block) => {
            update_item_defs_recurisve_stmts(
                &rust_expr_block.stmts,
                item_refs_all,
                item_defs_no_types,
                current_module,
                updated_item_defs,
                scoped_items,
                in_scope,
                duplicates,
            );
        }
        ExprRef::Break(_) => {}
        ExprRef::Call(_) => {}
        ExprRef::Cast(_) => {}
        ExprRef::Closure(_) => {}
        ExprRef::Const(_) => {}
        ExprRef::Continue(_) => {}
        ExprRef::Field(_) => {}
        ExprRef::ForLoop(_) => {}
        ExprRef::Group(_) => {}
        ExprRef::If(_) => {}
        ExprRef::Index(_) => {}
        ExprRef::Infer(_) => {}
        ExprRef::Let(_) => {}
        ExprRef::Lit(_) => {}
        ExprRef::Loop(_) => {}
        ExprRef::Macro(_) => {}
        ExprRef::Match(_) => {}
        ExprRef::MethodCall(_) => {}
        ExprRef::Paren(_) => {}
        ExprRef::Path(_) => {}
        ExprRef::Range(_) => {}
        ExprRef::Reference(_) => {}
        ExprRef::Repeat(_) => {}
        ExprRef::Return(_) => {}
        ExprRef::Struct(_) => {}
        ExprRef::Try(_) => {}
        ExprRef::TryBlock(_) => {}
        ExprRef::Tuple(_) => {}
        ExprRef::Unary(_) => {}
        ExprRef::Unsafe(_) => {}
        ExprRef::Verbatim(_) => {}
        ExprRef::While(_) => {}
        ExprRef::Yield(_) => {}
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

fn update_item_def(
    item: ItemActual,
    module_path: &[String],
    item_refs: &[ItemRef],
    item_actual_defs_copy: &[ItemActual],
    scoped_items: &[Vec<ItemRef>],
    in_scope: bool,
    duplicates: &[Duplicate],
) -> ItemV2 {
    match item {
        ItemActual::StructOrEnum(item_def) => {
            let _new_struct_or_enum_info = match item_def.struct_or_enum_info {
                StructOrEnumDefitionInfo::Struct(struct_def_info) => {
                    let fields = if struct_def_info.fields.is_empty() {
                        StructFieldInfo::UnitStruct
                    } else if struct_def_info
                        .fields
                        .iter()
                        .next()
                        .unwrap()
                        .ident
                        .is_some()
                    {
                        StructFieldInfo::RegularStruct(
                            struct_def_info
                                .fields
                                .iter()
                                .map(|f| {
                                    (
                                        f.ident.as_ref().unwrap().to_string(),
                                        // HERE IS THE PROBLEM
                                        parse_types_for_populate_item_definitions(
                                            &f.ty,
                                            &item_def.generics,
                                            module_path,
                                            item_refs,
                                            item_actual_defs_copy,
                                            scoped_items,
                                        ),
                                    )
                                })
                                .collect::<Vec<_>>(),
                        )
                    } else {
                        StructFieldInfo::TupleStruct(
                            struct_def_info
                                .fields
                                .iter()
                                .map(|f| {
                                    parse_types_for_populate_item_definitions(
                                        &f.ty,
                                        &item_def.generics,
                                        module_path,
                                        item_refs,
                                        item_actual_defs_copy,
                                        scoped_items,
                                    )
                                })
                                .collect::<Vec<_>>(),
                        )
                    };
                    StructOrEnumDefitionInfo2::Struct(StructDefinitionInfo {
                        fields,
                        syn_object: struct_def_info,
                    })
                }
                StructOrEnumDefitionInfo::Enum(enum_def_info) => {
                    let members_for_scope = enum_def_info
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
                                        item_actual_defs_copy,
                                        scoped_items,
                                    );
                                    match &f.ident {
                                        Some(input_name) => crate::update_item_definitions::EnumVariantInputsInfo::Named {
                                            ident: input_name.to_string(),
                                            input_type,
                                        },
                                        None => crate::update_item_definitions::EnumVariantInputsInfo::Unnamed(input_type),
                                    }
                                })
                                .collect::<Vec<_>>(),
                        })
                        .collect::<Vec<_>>();
                    StructOrEnumDefitionInfo2::Enum(EnumDefinitionInfo {
                        members: members_for_scope,
                        syn_object: enum_def_info,
                    })
                }
            };

            let js_name = if in_scope {
                Ident::String(item_def.ident.clone())
            } else if let Some(dup) = duplicates
                .iter()
                .find(|dup| item_def.ident == dup.name && dup.original_module_path == module_path)
            {
                Ident::Deduped(dup.namespace.clone())
            } else {
                Ident::String(item_def.ident.clone())
            };

            ItemV2::StructOrEnum(ItemDefinition {
                ident: item_def.ident.clone(),
                js_name,
                is_copy: item_def.is_copy,
                is_pub: item_def.is_pub,
                generics: item_def.generics,
                struct_or_enum_info: _new_struct_or_enum_info,
                impl_block_ids: Vec::new(),
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
                            item_actual_defs_copy,
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
                    item_actual_defs_copy,
                    scoped_items,
                ),
            };

            let js_name = if in_scope {
                Ident::Syn(fn_info.signature.ident.clone())
            } else {
                let in_module_level_duplicates = duplicates.iter().find(|dup| {
                    fn_info.signature.ident == dup.name && dup.original_module_path == module_path
                });

                if let Some(dup) = in_module_level_duplicates {
                    Ident::Deduped(dup.namespace.clone())
                } else {
                    Ident::Syn(fn_info.signature.ident.clone())
                }
            };

            ItemV2::Fn(FnInfo {
                ident: fn_info.ident.clone(),
                js_name,
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
                item_actual_defs_copy,
                scoped_items,
            );

            let js_name = if in_scope {
                Ident::String(const_def.name.clone())
            } else if let Some(dup) = duplicates
                .iter()
                .find(|dup| const_def.name == dup.name && dup.original_module_path == module_path)
            {
                Ident::Deduped(dup.namespace.clone())
            } else {
                Ident::String(const_def.name.clone())
            };

            ItemV2::Const(ConstDef {
                name: const_def.name.clone(),
                js_name,
                is_pub: const_def.is_pub,
                type_: rust_type,
                expr: const_def.expr,
            })
        }
        ItemActual::Trait(trait_def) => {
            // Currently trait defs don't store any info other than the name, so we don't need to do anything

            let js_name = if in_scope {
                Ident::String(trait_def.name.clone())
            } else {
                let in_module_level_duplicates = duplicates.iter().find(|dup| {
                    trait_def.name == dup.name && dup.original_module_path == module_path
                });

                if let Some(dup) = in_module_level_duplicates {
                    Ident::Deduped(dup.namespace.clone())
                } else {
                    Ident::String(trait_def.name.clone())
                }
            };
            ItemV2::Trait(RustTraitDefinition {
                name: trait_def.name.clone(),
                js_name,
                is_pub: trait_def.is_pub,
                syn: trait_def.syn,
                default_impls: trait_def
                    .default_impls
                    .into_iter()
                    .map(|fn_info| {
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
                                        item_actual_defs_copy,
                                        scoped_items,
                                    ),
                                ),
                            })
                            .collect::<Vec<_>>();

                        let return_type = match &fn_info.signature.output {
                            ReturnType::Default => RustType::Unit,
                            ReturnType::Type(_, type_) => {
                                parse_types_for_populate_item_definitions(
                                    type_,
                                    &fn_info.generics,
                                    module_path,
                                    item_refs,
                                    item_actual_defs_copy,
                                    scoped_items,
                                )
                            }
                        };

                        let js_name = if in_scope {
                            Ident::Syn(fn_info.signature.ident.clone())
                        } else {
                            let in_module_level_duplicates = duplicates.iter().find(|dup| {
                                fn_info.signature.ident == dup.name
                                    && dup.original_module_path == module_path
                            });

                            if let Some(dup) = in_module_level_duplicates {
                                Ident::Deduped(dup.namespace.clone())
                            } else {
                                Ident::Syn(fn_info.signature.ident.clone())
                            }
                        };

                        FnInfo {
                            ident: fn_info.ident.clone(),
                            js_name,
                            is_pub: fn_info.is_pub,
                            inputs_types,
                            generics: fn_info.generics,
                            return_type,
                            stmts: fn_info.stmts,
                            syn: fn_info.syn,
                        }
                    })
                    .collect(),
            })
        }
        ItemActual::Impl(item_impl, impl_items_refs) => {
            let impl_item_target_path = match &*item_impl.self_ty {
                Type::Path(type_path) => type_path
                    .path
                    .segments
                    .iter()
                    .map(|s| s.ident.to_string())
                    .collect::<Vec<_>>(),
                _ => todo!(),
            };

            let rust_impl_block_generics = item_impl
                .generics
                .params
                .iter()
                .filter_map(|gen| match gen {
                    GenericParam::Lifetime(_) => None,
                    GenericParam::Type(type_param) => Some(RustGeneric {
                        ident: type_param.ident.to_string(),
                        trait_bounds: type_param
                            .bounds
                            .iter()
                            .filter_map(|bound| {
                                // First lookup trait
                                match bound {
                                    TypeParamBound::Trait(trait_bound) => {
                                        let trait_path = trait_bound
                                            .path
                                            .segments
                                            .iter()
                                            .map(|seg| seg.ident.to_string());

                                        let (item_module_path, item_path, is_scoped, index) =
                                            resolve_path(
                                                // false,
                                                true,
                                                true,
                                                trait_path
                                                    .into_iter()
                                                    .map(|seg| RustPathSegment {
                                                        ident: seg.clone(),
                                                        turbofish: Vec::new(),
                                                    })
                                                    .collect::<Vec<_>>(),
                                                item_refs,
                                                item_actual_defs_copy,
                                                module_path,
                                                module_path,
                                                scoped_items,
                                            );

                                        Some(index.unwrap())
                                    }
                                    TypeParamBound::Lifetime(_) => None,
                                    TypeParamBound::Verbatim(_) => todo!(),
                                    _ => todo!(),
                                }
                            })
                            .collect::<Vec<_>>(),
                    }),
                    GenericParam::Const(_) => todo!(),
                })
                .collect::<Vec<_>>();

            // TODO fix this
            let target_type_param = match &*item_impl.self_ty {
                Type::Path(type_path) => {
                    if type_path.path.segments.len() == 1 {
                        rust_impl_block_generics
                            .iter()
                            .find(|generic| {
                                let first_seg =
                                    type_path.path.segments.first().unwrap().ident.to_string();
                                generic.ident == first_seg
                            })
                            .cloned()
                    } else {
                        None
                    }
                }
                // TODO handle other `Type`s properly
                _ => None,
            };

            let trait_index = item_impl.trait_.as_ref().map(|(_, trait_, _)| {
                let (item_module_path, item_path, is_scoped, index) = resolve_path(
                    // false,
                    true,
                    true,
                    trait_
                        .segments
                        .iter()
                        .map(|seg| RustPathSegment {
                            ident: seg.ident.to_string(),
                            turbofish: Vec::new(),
                        })
                        .collect::<Vec<_>>(),
                    item_refs,
                    item_actual_defs_copy,
                    module_path,
                    module_path,
                    scoped_items,
                );
                assert_eq!(item_path.len(), 1);
                (item_module_path, item_path[0].ident.clone(), index.unwrap())
            });

            let (target_rust_type, _is_target_type_param) =
                if let Some(target_type_param) = target_type_param {
                    (
                        RustType::TypeParam(RustTypeParam {
                            name: target_type_param.ident.clone(),
                            type_: RustTypeParamValue::Unresolved,
                        }),
                        true,
                    )
                } else {
                    // Get type of impl target

                    // dbg!(&module_path);
                    // dbg!(&temp_scope_id);
                    // dbg!(&impl_item_target_path);
                    // let (target_item_module, resolved_scope_id, target_item) = modules
                    //     .lookup_item_definition_any_module_or_scope(
                    //         &module_path,
                    //         &(!scope_id.is_empty()).then_some(scope_id.clone()),
                    //         &impl_item_target_path,
                    //     );
                    let (target_item_module, target_item, is_scoped, index) = resolve_path(
                        // false,
                        true,
                        true,
                        impl_item_target_path
                            .iter()
                            .map(|seg| RustPathSegment {
                                ident: seg.clone(),
                                turbofish: Vec::new(),
                            })
                            .collect::<Vec<_>>(),
                        item_refs,
                        item_actual_defs_copy,
                        module_path,
                        module_path,
                        scoped_items,
                    );
                    let target_item = target_item[0].clone();

                    // TODO Need to lookup item def to get generic params for RustType::StructOrEnum()? We shouldn't need to lookup the def. If the type is generic like `struct Foo<A, B> {}`, we get the type params or concrete types that are used for A and B from item_impl.self_ty and store these in RustType::StructOrEnum.

                    // TODO get rid of RustType::I32 etc, and just use StructOrEnum for everything
                    if target_item_module == [PRELUDE_MODULE_PATH] {
                        match &target_item.ident[..] {
                            "i32" => (RustType::I32, false),
                            _other => {
                                // TODO just defaulting to this because we want to get rid of all the special RustType variants anyway
                                (
                                    RustType::StructOrEnum(
                                        vec![],
                                        target_item_module.clone(),
                                        target_item.ident.to_string(),
                                        index.unwrap(),
                                    ),
                                    false,
                                )
                            }
                        }
                    } else {
                        (
                            RustType::StructOrEnum(
                                // target_item
                                //     .generics
                                //     .iter()
                                //     .map(|g| RustTypeParam {
                                //         name: g.clone(),
                                //         type_: RustTypeParamValue::Unresolved,
                                //     })
                                //     .collect::<Vec<_>>(),
                                vec![],
                                target_item_module.clone(),
                                target_item.ident.to_string(),
                                index.unwrap(),
                            ),
                            false,
                        )
                    }
                };

            // global_data.impl_block_target_type.pop();

            let rust_items = item_impl
                .items
                .iter()
                .zip(impl_items_refs)
                .map(|(syn_item, exprs_stmts_refs)| {
                    let item_name = match syn_item {
                        ImplItem::Const(_) => todo!(),
                        ImplItem::Fn(impl_item_fn) => impl_item_fn.sig.ident.to_string(),
                        ImplItem::Type(_) => todo!(),
                        ImplItem::Macro(_) => todo!(),
                        ImplItem::Verbatim(_) => todo!(),
                        _ => todo!(),
                    };

                    let rust_impl_item_item = match syn_item {
                        ImplItem::Const(_) => todo!(),
                        ImplItem::Fn(impl_item_fn) => {
                            let impl_block_generics =
                                rust_impl_block_generics.iter().map(|g| g.ident.clone());
                            let fn_generics = impl_item_fn
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
                            let combined_generics = impl_block_generics
                                .chain(fn_generics.iter().cloned())
                                .collect::<Vec<_>>();

                            let inputs_types = impl_item_fn
                                .sig
                                .inputs
                                .iter()
                                .map(|input| match input {
                                    FnArg::Receiver(receiver) => {
                                        // TODO need to actually parse the reciever to determine if it is boxed or a &mut so we can properly handle derefs
                                        // TODO need to ensure we are clear and consistent with the meaning of `RustType::ParentItem`
                                        // let rust_type = if receiver.reference.is_some()
                                        //     && receiver.mutability.is_some()
                                        // {
                                        //     RustType::MutRef(Box::new(RustType::ParentItem))
                                        // } else {
                                        //     RustType::ParentItem
                                        // };
                                        // (true, false, "self".to_string(), rust_type)

                                        let rust_type = if receiver.reference.is_some()
                                            && receiver.mutability.is_some()
                                        {
                                            RustType::MutRef(Box::new(target_rust_type.clone()))
                                        } else {
                                            target_rust_type.clone()
                                        };
                                        // (true, false, "self".to_string(), RustType::ParentItem)
                                        (true, false, "self".to_string(), rust_type)
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
                                            &combined_generics,
                                            &module_path,
                                            item_refs,
                                            item_actual_defs_copy,
                                            scoped_items,
                                        ),
                                    ),
                                })
                                .collect::<Vec<_>>();

                            let return_type = match &impl_item_fn.sig.output {
                                ReturnType::Default => RustType::Unit,
                                ReturnType::Type(_, type_) => {
                                    parse_types_for_populate_item_definitions(
                                        type_,
                                        &combined_generics,
                                        &module_path,
                                        item_refs,
                                        item_actual_defs_copy,
                                        scoped_items,
                                    )
                                }
                            };

                            // *scope_count += 1;
                            // scope_id.push(*scope_count);
                            // populate_impl_blocks_items_and_item_def_fields_stmts(
                            //     &impl_item_fn.block.stmts,
                            //     module,
                            //     global_data_copy,
                            //     module_path,
                            //     global_impl_blocks_simpl,
                            //     scope_id,
                            // );
                            // scope_id.pop();

                            let is_pub = match impl_item_fn.vis {
                                Visibility::Public(_) => true,
                                Visibility::Restricted(_) => todo!(),
                                Visibility::Inherited => false,
                            };

                            let js_name = Ident::Syn(impl_item_fn.sig.ident.clone());

                            RustImplItemItemNoJs::Fn(
                                {
                                    if let Some(input) = impl_item_fn.sig.inputs.first() {
                                        match input {
                                            FnArg::Receiver(_) => true,
                                            FnArg::Typed(_) => false,
                                        }
                                    } else {
                                        false
                                    }
                                },
                                FnInfo {
                                    js_name,
                                    ident: item_name.clone(),
                                    is_pub,
                                    inputs_types,
                                    generics: fn_generics,
                                    return_type,
                                    syn: FnInfoSyn::Impl(impl_item_fn.clone()),
                                    stmts: match exprs_stmts_refs {
                                        ImplItemExprStmtRefs::Fn(stmt_refs) => stmt_refs,
                                        ImplItemExprStmtRefs::Const => todo!(),
                                    },
                                },
                            )
                        }
                        ImplItem::Type(_) => todo!(),
                        ImplItem::Macro(_) => todo!(),
                        ImplItem::Verbatim(_) => todo!(),
                        _ => todo!(),
                    };
                    RustImplItemNoJs {
                        ident: item_name.clone(),
                        item: rust_impl_item_item,
                        // syn_object: syn_item.clone(),
                    }
                })
                .collect();

            // let js_name = if in_scope {
            //     Ident::String(item_def.ident.clone())
            // } else if let Some(dup) = duplicates.iter().find(|dup| {
            //     item_def.ident == dup.name && dup.original_module_path == module_path
            // }) {
            //     Ident::Deduped(dup.namespace.clone())
            // } else {
            //     Ident::String(item_def.ident.clone())
            // };

            ItemV2::Impl(RustImplBlockSimple {
                generics: rust_impl_block_generics,
                trait_: trait_index,
                target: target_rust_type.clone(),
                rust_items,
                syn: item_impl.clone(),
            })
        }
        ItemActual::None => panic!(),
    }
}

/// Similar to parse_fn_input_or_field but for the extract_data_populate_item_definitions() pass before parsing, so only dealing with top level items, so don't need to check for scoped item definitions, also given we are popualting `.item_definitions()` etc, we need to avoid using these. TODO IMPORTANT no I believe we are also dealing with scoped items in `extract_data_populate_item_definitions()`
///
/// Suitable for parsing: fn input types, fn return type, struct fields, enum variants with args
///
/// NOTE global data is required by get_path_without_namespacing which only uses pub_definitions etc, not `ItemDefintion`s
///
/// IMPORTANT NOTE this fn is never used in the first pass where item definitions are being recorded, only in the second pass where info about dependant types is being add, so we can safely lookup Path -> ItemDefinition here
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
    match type_ {
        Type::Array(_) => todo!(),
        Type::BareFn(_) => todo!(),
        Type::Group(_) => todo!(),
        Type::ImplTrait(type_impl_trait) => {
            debug!(type_ = ?type_, "parse_fn_input_or_field Type::ImplTrait");

            // We distinguish between normal traits which -> RustType::Impl, and fn/closure traits which -> RustType::Fn
            if type_impl_trait.bounds.len() == 1 {
                let bound = type_impl_trait.bounds.first().unwrap();
                match bound {
                    TypeParamBound::Trait(trait_bound) => {
                        if trait_bound.path.segments.len() == 1 {
                            let seg = trait_bound.path.segments.first().unwrap();
                            if seg.ident == "Fn" || seg.ident == "FnOnce" || seg.ident == "FnMut" {
                                return match &seg.arguments {
                                    PathArguments::None => todo!(),
                                    PathArguments::AngleBracketed(_) => todo!(),
                                    PathArguments::Parenthesized(args) => {
                                        let inputs = args
                                            .inputs
                                            .iter()
                                            .map(|input_type| {
                                                parse_types_for_populate_item_definitions(
                                                    input_type,
                                                    root_parent_item_definition_generics,
                                                    current_module,
                                                    item_refs,
                                                    item_defs,
                                                    scoped_items,
                                                )
                                            })
                                            .collect();
                                        let return_type = match &args.output {
                                            ReturnType::Default => RustType::Unit,
                                            ReturnType::Type(_, return_type) => {
                                                parse_types_for_populate_item_definitions(
                                                    return_type,
                                                    root_parent_item_definition_generics,
                                                    current_module,
                                                    item_refs,
                                                    item_defs,
                                                    scoped_items,
                                                )
                                            }
                                        };
                                        RustType::Closure(inputs, Box::new(return_type))
                                    }
                                };
                            }
                        }
                    }
                    TypeParamBound::Lifetime(_) => {}
                    TypeParamBound::Verbatim(_) => todo!(),
                    _ => todo!(),
                }
            }

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
                                                        item_refs,
                                                        item_defs,
                                                        scoped_items,
                                                    )
                                                }
                                                GenericArgument::Const(_) => todo!(),
                                                GenericArgument::AssocType(_) => todo!(),
                                                GenericArgument::AssocConst(_) => todo!(),
                                                GenericArgument::Constraint(_) => todo!(),
                                                _ => todo!(),
                                            })
                                            .collect::<Vec<_>>(),
                                        PathArguments::Parenthesized(_) => {
                                            dbg!(seg);
                                            todo!();
                                        }
                                    },
                                })
                                .collect::<Vec<_>>();

                            // TODO lookup trait in global data to get module path
                            let (trait_module_path, trait_item_path, trait_item_scope, item_index) =
                                make_item_definitions::resolve_path(
                                    true,
                                    true,
                                    trait_bound_path,
                                    item_refs,
                                    item_defs,
                                    current_module,
                                    current_module,
                                    scoped_items,
                                );
                            // A Trait bound should just be a trait, no associated fn or whatever
                            assert!(trait_item_path.len() == 1);

                            // let (module_path, trait_definition) = global_data
                            //     .lookup_trait_definition_any_module(&trait_name, current_module)
                            //     .unwrap();
                            Some(RustTypeImplTrait::SimpleTrait(item_index.unwrap()))
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

            // dbg!(seg_name_str);

            // For impl blocks
            match seg_name_str {
                // TODO Option should be added to module/global data so we can handle it like any other item and also handle it properly if is has been shadowed
                "Option" => {
                    todo!();
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
                                        item_refs,
                                        item_defs,
                                        scoped_items,
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
                    RustType::Option(RustTypeParam {
                        // TODO "T" shouldn't be hardcoded here
                        name: "T".to_string(),
                        type_: RustTypeParamValue::RustType(Box::new(generic_type)),
                    })
                }
                "Result" => {
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
                                        item_refs,
                                        item_defs,
                                        scoped_items,
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
                    RustType::Result(RustTypeParam {
                        // TODO "T" shouldn't be hardcoded here
                        name: "T".to_string(),
                        type_: RustTypeParamValue::RustType(Box::new(generic_type)),
                    })
                }
                _ => {
                    // get full path
                    // NOTE only the final segment should have turbofish, or the final two if the path is an associated item
                    // NOTE also, get_path_without_namespacing() only preserves `RustPathSeg`s/turbofish, it doesn't use or update them so we could just populate them later
                    // dbg!("parse type path");
                    // println!("{}", quote! { #type_path });

                    let rust_path = type_path
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
                                    .enumerate()
                                    .filter_map(|(_i, arg)| match arg {
                                        GenericArgument::Lifetime(_) => None,
                                        GenericArgument::Type(arg_type_) => {
                                            Some(parse_types_for_populate_item_definitions(
                                                arg_type_,
                                                root_parent_item_definition_generics,
                                                current_module,
                                                item_refs,
                                                item_defs,
                                                scoped_items,
                                            ))
                                        }
                                        GenericArgument::Const(_) => todo!(),
                                        GenericArgument::AssocType(_) => todo!(),
                                        GenericArgument::AssocConst(_) => todo!(),
                                        GenericArgument::Constraint(_) => todo!(),
                                        _ => todo!(),
                                    })
                                    .collect(),
                                PathArguments::Parenthesized(_) => todo!(),
                            },
                        })
                        .collect();

                    // TODO important should replace get_path with item lookup like below
                    // let (item_definition_module_path, resolved_scope_id, item_definition) =
                    //     global_data.lookup_item_definition_any_module_or_scope(
                    //         current_module,
                    //         &global_data.scope_id_as_option(),
                    //         &vec![struct_or_enum_name.to_string()],
                    //     );
                    let (item_module_path, item_path_seg, item_scope, item_index) =
                        make_item_definitions::resolve_path(
                            true,
                            true,
                            rust_path,
                            item_refs,
                            item_defs,
                            current_module,
                            current_module,
                            scoped_items,
                        );
                    let item_seg = &item_path_seg[0];

                    let mut type_params = item_seg
                        .turbofish
                        .iter()
                        .map(|rt| RustTypeParam {
                            name: "unknown_todo".to_string(),
                            type_: RustTypeParamValue::RustType(Box::new(rt.clone())),
                        })
                        .collect::<Vec<_>>();

                    if item_module_path == vec!["prelude_special_case".to_string()] {
                        if item_seg.ident == "i32" {
                            // if has_mut_keyword {
                            //     global_data.rust_prelude_types.rust_integer = true;
                            // }
                            RustType::I32
                        } else if item_seg.ident == "String" || item_seg.ident == "str" {
                            // if has_mut_keyword {
                            //     global_data.rust_prelude_types.rust_string = true;
                            // }
                            RustType::String
                        } else if item_seg.ident == "bool" {
                            // if has_mut_keyword {
                            //     global_data.rust_prelude_types.rust_string = true;
                            // }
                            RustType::Bool
                        } else if item_seg.ident == "Vec" {
                            // if has_mut_keyword {
                            //     global_data.rust_prelude_types.rust_string = true;
                            // }
                            // dbg!(&type_params);
                            // dbg!(&type_path.path.segments);
                            assert_eq!(type_params.len(), 1);
                            RustType::Vec(Box::new(RustType::TypeParam(type_params.remove(0))))
                        } else {
                            dbg!(&item_seg.ident);
                            todo!()
                        }
                    } else {
                        // NOTE for now we are assuming the type must be a struct or enum. fn() types will get matched by Type::BareFn not Type::Path, and traits should only appear in Type::ImplTrait. However we need to handle associated items eg `field: <MyStruct as MyTrait>::some_associated_type` which is a Path but to a type, not necessarily a struct/enum.
                        RustType::StructOrEnum(
                            type_params,
                            item_module_path,
                            item_seg.ident.clone(),
                            item_index.unwrap(),
                        )
                    }
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
                item_refs,
                item_defs,
                scoped_items,
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

// #[allow(dead_code)]
// #[derive(Debug, Clone, PartialEq, Eq)]
// enum RustTypeImplTrait {
//     SimpleTrait(String),
//     /// (return type)
//     Fn(RustType),
// }

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
    scoped_items: &[Vec<ItemRef>],
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
                                            // dbg!(&seg_new);
                                            // dbg!(&def.ident);
                                            (def.ident == seg_new).then_some(*index)
                                        }
                                        _ => todo!(),
                                    }
                                }
                                ItemRef::Fn(index) => {
                                    let item = &items_defs[*index];
                                    match item {
                                        ItemActual::Fn(def) => {
                                            (def.ident == seg_new).then_some(*index)
                                        }
                                        _ => todo!(),
                                    }
                                }
                                ItemRef::Const(index) => {
                                    let item = &items_defs[*index];
                                    match item {
                                        ItemActual::Const(def) => {
                                            (def.name == seg_new).then_some(*index)
                                        }
                                        _ => todo!(),
                                    }
                                }
                                ItemRef::Trait(index) => {
                                    let item = &items_defs[*index];
                                    match item {
                                        ItemActual::Trait(def) => {
                                            (def.name == seg_new).then_some(*index)
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
pub enum StructOrEnumDefitionInfo2 {
    Struct(StructDefinitionInfo),
    Enum(EnumDefinitionInfo),
}

/// Similar to StructOrEnum which gets used in RustType, but is for storing info about the actual item definition, rather than instances of, so eg we don't need to be able to store resolved generics. Minor differences but making distinct type helps with reasoning about the different use cases.
/// Just structs and enums or should we include functions?
#[derive(Debug, Clone)]
pub struct ItemDefinition {
    pub js_name: Ident,
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
    pub struct_or_enum_info: StructOrEnumDefitionInfo2,
    // impl_blocks: Vec<ItemDefintionImpls>,
    /// Should be used for matching only inherent impls to the target type, which is always be a struct or enum (or type alias of a struct or enum)? but trait impls also currently are added
    ///
    /// Currently used for:
    /// -   looking up impl items
    /// -   add static fields (pointing to trait class) and methods to JsClass (update_classes_stmts())
    /// -   adding stmts like `Number.prototype.foo = bar.prototype.foo` (handle_syn_item())
    ///
    /// These are uuids/references to all the impl blocks whose target match this struct/enum
    ///
    /// (unique impl id)
    // TODO use reference instead of id?
    pub impl_block_ids: Vec<usize>,
}
impl ItemDefinition {
    pub fn get_type(&self, field_member: &Member, global_data: &GlobalData) -> RustType2 {
        match &self.struct_or_enum_info {
            StructOrEnumDefitionInfo2::Struct(struct_def_info) => match &struct_def_info.fields {
                StructFieldInfo::UnitStruct => todo!(),
                StructFieldInfo::TupleStruct(_) => todo!(),
                StructFieldInfo::RegularStruct(fields2) => fields2
                    .iter()
                    .find_map(|(field_name, field_type)| {
                        let field_member_name = match field_member {
                            Member::Named(ident) => ident.to_string(),
                            Member::Unnamed(_) => todo!(),
                        };
                        (field_name == &field_member_name)
                            .then_some(field_type.clone().into_rust_type2(global_data))
                    })
                    .unwrap()
                    .clone(),
            },
            StructOrEnumDefitionInfo2::Enum(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RustTraitDefinition {
    pub js_name: Ident,
    pub name: String,
    pub is_pub: bool,
    // impl_items:
    pub syn: ItemTrait,
    pub default_impls: Vec<FnInfo>,
}

// Do we want to also store the trait bounds of each type param? This way if we have an unresolved type param that calls some function, we will know what trait to look up to find it. In some cases this might also remove the need for looking forward to resolve type params, if all we need to do with the type param/value/intance/type is call a method on it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustTypeParam {
    pub name: String,
    pub type_: RustTypeParamValue,
}
impl RustTypeParam {
    pub fn into_rust_type_param2(self, global_data: &GlobalData) -> RustTypeParam2 {
        RustTypeParam2 {
            name: self.name,
            type_: match self.type_ {
                RustTypeParamValue::Unresolved => RustTypeParamValue2::Unresolved,
                RustTypeParamValue::RustType(type_) => {
                    RustTypeParamValue2::RustType(Box::new(type_.into_rust_type2(global_data)))
                }
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustTypeParamValue {
    /// Can't be known at this point in analysis, eg the type is inferred somewhere else in the code
    Unresolved,
    RustType(Box<RustType>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustTypeImplTrait {
    SimpleTrait(usize),
    /// (return type)
    Fn(RustType),
}

#[derive(Debug, Clone)]
pub struct RustImplItemNoJs {
    pub ident: String,
    pub item: RustImplItemItemNoJs,
    // return_type: RustType,
    // syn_object: ImplItem,
}

#[derive(Debug, Clone)]
pub enum RustImplItemItemNoJs {
    /// (static, fn info),
    Fn(bool, FnInfo),
    Const,
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
// NOTE we include specialised types like RustType::I32 for performance reasons to avoid needing to do a comparison like `module_path == [...]` everytime we want to check for an integer or string which is often given they are the most common types
// NOTE we need ParentItem because we can use `Self` in expressions like `let foo = Self { foo: 5 };` etc, but we should avoid using it in definitions, only for syn parsing/expression code
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustType {
    /// For cases/expressions we know cannot return a type, eg `break`
    NotAllowed,
    /// Can't be known at this point in analysis, eg the type is inferred somewhere else in the code
    Unknown,
    // /// Unitialized/null
    // Uninit,
    /// Needs implementing
    Todo,
    // Self_,
    /// I think ParentItem means it is actually `self` not just `Self`???
    /// NOTE ParentItem is always self or Self, and these keywords are *always* referring to the target in an impl block, so if we come across a RustType::ParentItem we can determine what it is by looking up the global_data.impl_target or whatever it is called
    /// NOTE `Self` can also be used directly in a eg struct def like `struct Foo(Box<Self>);`. We are not currently handling/supporting these cases but need to bear this in mind for `RustType::ParentItem`
    /// NOTE if ParentItem is returned by an impl item fn it must be immediately converted to the receiver type so that we can be sure that we are in a static fn/def when parsing and we come across a ParentItem
    // ParentItem,
    /// ()
    Unit,
    /// !
    Never,
    /// Fns might return impl FooTrait, and that will also be the type of eg any var that the result is assigned to. It's fine not knowing the exact type because you can only call the trait's methods on it, but need to be able to look up the trait's methods to know what type they return.
    ///
    /// Vec<(module path, scope id, RustTypeImplTrait (ie simple FooTrait or a fn trait: Fn(i32) -> i32))>
    ImplTrait(Vec<RustTypeImplTrait>),
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
    /// The RustType for option *must* be a type param. (I believe) It is important to always have a type param to ensure we can always lookup the name of the generic to match against generics in methods etc NO, we can always lookup the name of the generic on the item_def
    Option(RustTypeParam),
    // Option(Box<RustType>),
    /// (generic)
    Result(RustTypeParam),
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
    /// (type params, module path, scope id, name)
    /// (type params, module path, name, index)
    StructOrEnum(Vec<RustTypeParam>, Vec<String>, String, usize),
    // Struct(Vec<RustTypeParam>, Vec<String>, String),
    /// (type params, module path, name)  
    // Enum(Vec<RustTypeParam>, Vec<String>, String),
    // TODO Should we use the same type for both Arrays and Vecs, because they get transpiled to the same thing anyway? NO because we need to handle the types differently, ie arrays need `.copy()` adding when they are moved (although this won't be necessary if the previous value is not used after the move/copy, but this would be hard to determine so need to just always add copy).
    Vec(Box<RustType>),
    Array(Box<RustType>),
    Tuple(Vec<RustType>),
    /// Even though Box::new() vanishes when transpiled, we need to keep track of which vars are Boxed because the dereferencing behaves differently
    Box(Box<RustType>),
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
    /// (item type params, type params, module path, scope id, name)
    Fn(
        Option<Vec<RustTypeParam>>,
        Vec<RustTypeParam>,
        Vec<String>,
        // TODO arguably it would be better to just store the path and item name all in one, and when looking up the item/fn we are able to determine at that point whether the final one or two elements of the path are a item or associated fn or whatever
        RustTypeFnType,
        usize,
    ),
    /// For things like Box::new where we want `Box::new(1)` -> `1`
    FnVanish,
    /// We need a separate type for closures because there is no definition with a path/ident to look up like RustType::Fn. Maybe another reason to store the type info directly and avoid using lookups so we don't need two separate variants.
    /// (input types, return type)
    Closure(Vec<RustType>, Box<RustType>),
}
impl RustType {
    pub fn into_rust_type2(self, global_data: &GlobalData) -> RustType2 {
        match self {
            RustType::NotAllowed => RustType2::NotAllowed,
            RustType::Unknown => RustType2::Unknown,
            RustType::Todo => RustType2::Todo,
            RustType::Unit => RustType2::Unit,
            RustType::Never => RustType2::Never,
            RustType::ImplTrait(traits) => RustType2::ImplTrait(
                traits
                    .into_iter()
                    .map(|trait_| match trait_ {
                        RustTypeImplTrait::SimpleTrait(index) => {
                            let trait_def = global_data.get_trait(index);
                            RustTypeImplTrait2::SimpleTrait(trait_def)
                        }
                        RustTypeImplTrait::Fn(_) => todo!(),
                    })
                    .collect(),
            ),
            RustType::TypeParam(rust_type_param) => {
                RustType2::TypeParam(rust_type_param.into_rust_type_param2(global_data))
            }
            RustType::I32 => RustType2::I32,
            RustType::F32 => RustType2::F32,
            RustType::Bool => RustType2::Bool,
            RustType::String => RustType2::String,
            RustType::Option(rust_type_param) => {
                RustType2::Option(rust_type_param.into_rust_type_param2(global_data))
            }
            RustType::Result(rust_type_param) => {
                RustType2::Result(rust_type_param.into_rust_type_param2(global_data))
            }
            RustType::StructOrEnum(type_params, module_path, name, index) => {
                let item_def = global_data.get_struct_enum(index);
                RustType2::StructOrEnum(
                    type_params
                        .into_iter()
                        .map(|tp| tp.into_rust_type_param2(global_data))
                        .collect(),
                    item_def,
                )
            }
            RustType::Vec(inner) => RustType2::Vec(Box::new(inner.into_rust_type2(global_data))),
            RustType::Array(inner) => {
                RustType2::Array(Box::new(inner.into_rust_type2(global_data)))
            }
            RustType::Tuple(inner) => RustType2::Tuple(
                inner
                    .into_iter()
                    .map(|type_| type_.into_rust_type2(global_data))
                    .collect(),
            ),
            RustType::Box(inner) => RustType2::Box(Box::new(inner.into_rust_type2(global_data))),
            RustType::UserType(name, inner) => {
                RustType2::UserType(name, Box::new(inner.into_rust_type2(global_data)))
            }
            RustType::MutRef(inner) => {
                RustType2::MutRef(Box::new(inner.into_rust_type2(global_data)))
            }
            RustType::Ref(inner) => RustType2::Ref(Box::new(inner.into_rust_type2(global_data))),
            RustType::Fn(item_type_params, type_params, module_path, name, index) => {
                let fn_info = global_data.get_fn(index);
                RustType2::Fn(
                    item_type_params.map(|tps| {
                        tps.into_iter()
                            .map(|tp| tp.into_rust_type_param2(global_data))
                            .collect()
                    }),
                    type_params
                        .into_iter()
                        .map(|tp| tp.into_rust_type_param2(global_data))
                        .collect(),
                    Box::new(fn_info),
                )
            }
            RustType::FnVanish => RustType2::FnVanish,
            RustType::Closure(inputs, return_) => RustType2::Closure(
                inputs
                    .into_iter()
                    .map(|type_| type_.into_rust_type2(global_data))
                    .collect(),
                Box::new(return_.into_rust_type2(global_data)),
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustTypeFnType {
    /// (fn name)
    Standalone(String),
    /// (item name, fn name)
    AssociatedFn(String, String),
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub js_name: Ident,
    pub name: String,
    pub is_pub: bool,
    pub type_: RustType,
    pub expr: ExprRef,
}

/// Not just for methods, can also be an enum variant with no inputs
#[derive(Debug, Clone)]
pub struct FnInfo {
    // TODO No point storing all the info like inputs and return types separately, as these need to be stored on RustType::Fn anyway for eg closures where we won't be storing a fn info?? Keep both for now and revisit later. Note fns idents can just appear in the code and be called whereas a closure will be a var which already has a type.
    pub js_name: Ident,
    pub ident: String,
    pub is_pub: bool,
    /// Does this include receiver/self types? NO in handle_item_fn we are filtering out any self type. Could just store it as RustType::Self, but seems pointless if we don't actually need it for anything. NO again, updated to include self inputs because we need them.
    /// TODO probably don't actually need `is_self`
    /// (is_self, is_mut, name, type)
    pub inputs_types: Vec<(bool, bool, String, RustType)>,
    pub generics: Vec<String>,
    // NO! for methods we want to store the actual fn type. fns can be assigned to vars, and we want to be able to pass the Path part of the fn, and *then* call it and determine the return type
    pub return_type: RustType,
    // /// type of fn eg Fn(i32) -> ()
    // rust_type: RustType,
    // TODO optionally add enum for Field, AssociatedFn, Method, etc
    pub syn: FnInfoSyn,

    pub stmts: Vec<StmtsRef>,
}

impl FnInfo {
    pub fn attempt_to_resolve_type_params_using_arg_types(
        &self,
        args: &[RustType2],
    ) -> Vec<RustTypeParam2> {
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
                        RustTypeParamValue2::RustType(Box::new(matched_arg_rust_type))
                    } else {
                        RustTypeParamValue2::Unresolved
                    };

                RustTypeParam2 {
                    name: g.clone(),
                    type_: rust_type_param_value,
                }
            })
            .collect::<Vec<_>>()
    }
}

#[derive(Debug, Clone)]
pub struct RustGeneric {
    pub ident: String,
    // (module path, trait name)
    // pub trait_bounds: Vec<(Vec<String>, Option<Vec<usize>>, String)>,
    pub trait_bounds: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct RustImplBlockSimple {
    // pub unique_id: String,
    // TODO Should this include generics that are defined on the target type, or just new generics introduced for the impl Trait or used in the methods/items? For now just assume it is everything.
    pub generics: Vec<RustGeneric>,
    /// NOTE we include module and name for generating a unique name for the impl block. TODO probably not best approach, what about identical signature?
    /// (module, name, index)
    pub trait_: Option<(Vec<String>, String, usize)>,
    // Note this can a generic param
    pub target: RustType,
    pub rust_items: Vec<RustImplItemNoJs>,
    // items: Vec<ImplItem>,
    pub syn: ItemImpl,
}
