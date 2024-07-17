use syn::{
    Expr, FnArg, GenericParam, ImplItem, Item, Pat, ReturnType, Stmt, Type, TypeParamBound,
    Visibility,
};
use tracing::debug_span;

use crate::{
    get_item_impl_unique_id,
    make_item_definitions::{
        EnumVariantInfo, EnumVariantInputsInfo, FnInfo, FnInfoSyn, ModuleData, StructFieldInfo,
        StructOrEnumDefitionInfo, VariousDefintions,
    },
    parse_types_for_populate_item_definitions, GlobalData, RustGeneric, RustImplBlockSimple,
    RustImplItemItemNoJs, RustImplItemNoJs, RustType, RustTypeParam, RustTypeParamValue,
};

pub fn update_item_definitions(global_data: &mut GlobalData) {
    let global_data_copy = global_data.clone();

    let mut global_impl_blocks_simpl = Vec::new();

    for module in &mut global_data.modules {
        debug_span!(
            "extract_data_populate_item_definitions module: {:?}",
            module_path = ?module.path
        );
        let module_path = module.path.clone();
        let items = module.items.clone();
        // populate_impl_blocks_items_and_item_def_fields(
        //     &items,
        //     module,
        //     &global_data_copy,
        //     &module_path,
        //     &mut global_data.impl_blocks_simpl,
        //     // &mut module.scoped_various_definitions,
        //     &mut scope_id,
        // );

        update_various_def(
            &mut module.various_definitions,
            &global_data_copy,
            &module_path,
            &mut global_impl_blocks_simpl,
            &None,
        );

        for (scope, various_def) in &mut module.scoped_various_definitions {
            update_various_def(
                various_def,
                &global_data_copy,
                &module_path,
                &mut global_impl_blocks_simpl,
                &Some(scope.clone()),
            );
        }

        for (scope_id, item_impl) in &module.scoped_syn_impl_items {
            // Temporarily store impl block's type params on global data

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

                                        let (module_path, scope_id, trait_def) = global_data_copy
                                            .lookup_trait_definition_any_module(
                                                &module_path,
                                                &(!scope_id.is_empty()).then_some(scope_id.clone()),
                                                trait_path,
                                            );
                                        Some((module_path, scope_id, trait_def.name))
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

            let trait_path_and_name = item_impl.trait_.as_ref().map(|(_, trait_, _)| {
                let (module_path, trait_scope_id, trait_def) = global_data_copy
                    .lookup_trait_definition_any_module(
                        &module_path,
                        &(!scope_id.is_empty()).then_some(scope_id.clone()),
                        trait_.segments.iter().map(|seg| seg.ident.to_string()),
                    );
                (module_path, trait_scope_id, trait_def.name)
            });

            let (target_rust_type, is_target_type_param) =
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
                    let (target_item_module, resolved_scope_id, target_item) = global_data_copy
                        .lookup_item_definition_any_module_or_scope(
                            &module_path,
                            &(!scope_id.is_empty()).then_some(scope_id.clone()),
                            &impl_item_target_path,
                        );
                    // dbg!("here");
                    (
                        RustType::StructOrEnum(
                            target_item
                                .generics
                                .iter()
                                .map(|g| RustTypeParam {
                                    name: g.clone(),
                                    type_: RustTypeParamValue::Unresolved,
                                })
                                .collect::<Vec<_>>(),
                            target_item_module.clone(),
                            resolved_scope_id,
                            target_item.ident.to_string(),
                        ),
                        false,
                    )
                };

            // global_data.impl_block_target_type.pop();

            let rust_items = item_impl
                .items
                .iter()
                .map(|syn_item| {
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
                                    FnArg::Receiver(_) => {
                                        // TODO need to actually parse the reciever to determine if it is boxed or a &mut so we can properly handle derefs
                                        // TODO need to ensure we are clear and consistent with the meaning of `RustType::ParentItem`
                                        (true, false, "self".to_string(), RustType::ParentItem)
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
                                            &(!scope_id.is_empty()).then_some(scope_id.clone()),
                                            &global_data_copy,
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
                                        &(!scope_id.is_empty()).then_some(scope_id.clone()),
                                        &global_data_copy,
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
                                    ident: item_name.clone(),
                                    is_pub,
                                    inputs_types,
                                    generics: fn_generics,
                                    return_type,
                                    syn: FnInfoSyn::Impl(impl_item_fn.clone()),
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

            global_impl_blocks_simpl.push(RustImplBlockSimple {
                unique_id: get_item_impl_unique_id(
                    &module_path,
                    &(!scope_id.is_empty()).then_some(scope_id.clone()),
                    &item_impl,
                ),
                generics: rust_impl_block_generics,
                trait_: trait_path_and_name,
                target: target_rust_type.clone(),
                rust_items,
            });
        }

        // for item in items {
        //     // dbg!("populate_impl_blocks_items");
        //     // println!("{}", quote! { #item });
        //     populate_impl_blocks_items_and_item_def_fields_individual(
        //         &item,
        //         module,
        //         &global_data_copy,
        //         &module_path,
        //         &mut global_data.impl_blocks_simpl,
        //         &mut scope_id,
        //         &mut scope_count,
        //     );
        // }

        // Add unique impl block ids to item_def.impl_block_ids
        // NOTE it is ok to do this in the same pass as populate_impl_blocks_items_and_item_def_fields because we are only relying on the traits defined in the impl block signature... NO it won't work because we are literally creating the `RustImplBlockSimple`s in populate_impl_blocks_items_and_item_def_fields so they won't all exist until the end of the loop over modules
        // populate_item_def_impl_blocks(&mut global_data);
        // update_item_def_block_ids(...
    }

    global_data.impl_blocks_simpl = global_impl_blocks_simpl;
}

// fn populate_impl_blocks_items_and_item_def_fields(
//     items: &Vec<Item>,
//     module: &mut ModuleData,
//     global_data_copy: &GlobalData,
//     module_path: &[String],
//     global_impl_blocks_simpl: &mut Vec<RustImplBlockSimple>,
//     // scoped_various_definitions: &mut Vec<(Vec<usize>, VariousDefintions, Vec<RustImplBlockSimple>)>,
//     scope_id: &mut Vec<usize>,
// ) {
//     let mut scope_count = 0;

//     for item in items {
//         // dbg!("populate_impl_blocks_items");
//         // println!("{}", quote! { #item });
//         populate_impl_blocks_items_and_item_def_fields_individual(
//             item,
//             module,
//             global_data_copy,
//             module_path,
//             global_impl_blocks_simpl,
//             scope_id,
//             &mut scope_count,
//         );
//     }
// }
fn populate_impl_blocks_items_and_item_def_fields_stmts(
    stmts: &Vec<Stmt>,
    module: &mut ModuleData,
    global_data_copy: &GlobalData,
    module_path: &[String],
    global_impl_blocks_simpl: &mut Vec<RustImplBlockSimple>,
    // scoped_various_definitions: &mut Vec<(Vec<usize>, VariousDefintions, Vec<RustImplBlockSimple>)>,
    scope_id: &mut Vec<usize>,
) {
    let mut scope_count = 0;

    // dbg!("populate_impl_blocks_items");
    // println!("{}", quote! { #item });

    for stmt in stmts {
        match stmt {
            Stmt::Local(local) => {
                let init = local.init.as_ref().unwrap();
                populate_impl_blocks_items_and_item_def_fields_expr(
                    &init.expr,
                    module,
                    global_data_copy,
                    module_path,
                    global_impl_blocks_simpl,
                    scope_id,
                    &mut scope_count,
                    false,
                );
            }
            Stmt::Item(item) => {
                populate_impl_blocks_items_and_item_def_fields_individual(
                    item,
                    module,
                    global_data_copy,
                    module_path,
                    global_impl_blocks_simpl,
                    scope_id,
                    &mut scope_count,
                );
            }
            Stmt::Expr(expr, _) => {
                populate_impl_blocks_items_and_item_def_fields_expr(
                    expr,
                    module,
                    global_data_copy,
                    module_path,
                    global_impl_blocks_simpl,
                    scope_id,
                    &mut scope_count,
                    false,
                );
            }
            Stmt::Macro(_) => {}
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn populate_impl_blocks_items_and_item_def_fields_expr(
    expr: &Expr,
    module: &mut ModuleData,
    global_data_copy: &GlobalData,
    module_path: &[String],
    global_impl_blocks_simpl: &mut Vec<RustImplBlockSimple>,
    // scoped_various_definitions: &mut Vec<(Vec<usize>, VariousDefintions, Vec<RustImplBlockSimple>)>,
    scope_id: &mut Vec<usize>,
    scope_count: &mut usize,
    force_new_scope: bool,
) {
    // dbg!("populate_impl_blocks_items");
    // println!("{}", quote! { #item });

    fn forced_inc_scope_count_and_id_and_push_empty_scope(
        force_new_scope: bool,
        scope_count: &mut usize,
        scope_id: &mut Vec<usize>,
    ) {
        if force_new_scope {
            *scope_count += 1;
            scope_id.push(*scope_count);
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

            populate_impl_blocks_items_and_item_def_fields_stmts(
                &expr_async.block.stmts,
                module,
                global_data_copy,
                module_path,
                global_impl_blocks_simpl,
                scope_id,
            );

            scope_id.pop();
        }
        Expr::Await(_) => {}
        Expr::Binary(_) => {}
        Expr::Block(expr_block) => {
            *scope_count += 1;
            scope_id.push(*scope_count);

            populate_impl_blocks_items_and_item_def_fields_stmts(
                &expr_block.block.stmts,
                module,
                global_data_copy,
                module_path,
                global_impl_blocks_simpl,
                scope_id,
            );

            scope_id.pop();
        }
        Expr::Break(_) => {}
        Expr::Call(_) => {}
        Expr::Cast(_) => {}
        Expr::Closure(expr_closure) => {
            populate_impl_blocks_items_and_item_def_fields_expr(
                &expr_closure.body,
                module,
                global_data_copy,
                module_path,
                global_impl_blocks_simpl,
                scope_id,
                scope_count,
                true,
            );
        }
        Expr::Const(_) => {}
        Expr::Continue(_) => {}
        Expr::Field(_) => {}
        Expr::ForLoop(_) => {}
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
            );
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Loop(_) => {}
        Expr::Macro(_) => {}
        Expr::Match(expr_match) => {
            // We wouldn't normally create a new scope for a match expression but if it is the body of eg a closure or a another match expression's arm body, then force_new_scope will be true and we must create an empty scope (NOTE this is different to the arm body scopes which are created below, this is a single empty scope for the entire match expression)
            forced_inc_scope_count_and_id_and_push_empty_scope(
                force_new_scope,
                scope_count,
                scope_id,
            );

            populate_impl_blocks_items_and_item_def_fields_expr(
                &expr_match.expr,
                module,
                global_data_copy,
                module_path,
                global_impl_blocks_simpl,
                scope_id,
                scope_count,
                false,
            );
            // TODO We count in reverse to match the JS parsing but should probably try and fix this so we don't have to remember that match arm body scopes are counted backwards
            for arm in expr_match.arms.iter().rev() {
                // In this case, even if the arm body is simply a path like `x`, we still need to create a scope because the `x` can be an argument of eg the enum variant for this match arm.
                populate_impl_blocks_items_and_item_def_fields_expr(
                    &arm.body,
                    module,
                    global_data_copy,
                    module_path,
                    global_impl_blocks_simpl,
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
            );
            populate_impl_blocks_items_and_item_def_fields_expr(
                &expr_method_call.receiver,
                module,
                global_data_copy,
                module_path,
                global_impl_blocks_simpl,
                scope_id,
                scope_count,
                false,
            );
            for expr in &expr_method_call.args {
                populate_impl_blocks_items_and_item_def_fields_expr(
                    expr,
                    module,
                    global_data_copy,
                    module_path,
                    global_impl_blocks_simpl,
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
            );
            drop_forced_empty_scope(force_new_scope, scope_id);
        }
        Expr::Range(_) => {}
        Expr::Reference(_) => {}
        Expr::Repeat(_) => {}
        Expr::Return(_) => {}
        Expr::Struct(_) => {}
        Expr::Try(_) => {}
        Expr::TryBlock(_) => {}
        Expr::Tuple(_) => {}
        Expr::Unary(_) => {}
        Expr::Unsafe(_) => {}
        Expr::Verbatim(_) => {}
        Expr::While(_) => {}
        Expr::Yield(_) => {}
        _ => {}
    }
}

fn update_various_def(
    // item: &Item,
    various_definition: &mut VariousDefintions,
    // module: &mut ModuleData,
    global_data_copy: &GlobalData,
    module_path: &[String],
    global_impl_blocks_simpl: &mut Vec<RustImplBlockSimple>,
    // scoped_various_definitions: &mut Vec<(Vec<usize>, VariousDefintions, Vec<RustImplBlockSimple>)>,
    // scope_id: &mut Vec<usize>,
    // scope_count: &mut usize,
    current_scope: &Option<Vec<usize>>,
) {
    for const_def in &mut various_definition.consts {
        let rust_type = parse_types_for_populate_item_definitions(
            &const_def.syn_object.ty,
            &Vec::new(),
            module_path,
            current_scope,
            global_data_copy,
        );

        const_def.type_ = rust_type;
    }

    for item_def in &mut various_definition.item_definitons {
        match &mut item_def.struct_or_enum_info {
            StructOrEnumDefitionInfo::Struct(struct_def_info) => {
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
                                        current_scope,
                                        global_data_copy,
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
                                    &current_scope,
                                    global_data_copy,
                                )
                            })
                            .collect::<Vec<_>>(),
                    )
                };
                struct_def_info.fields = fields;
            }
            StructOrEnumDefitionInfo::Enum(enum_def_info) => {
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
                                    current_scope,
                                    global_data_copy,
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
                enum_def_info.members = members_for_scope;
            }
        }
    }

    for fn_info in &mut various_definition.fn_info {
        let item_fn = match &fn_info.syn {
            FnInfoSyn::Standalone(item_fn) => item_fn,
            FnInfoSyn::Impl(_) => todo!(),
        };
        let inputs_types = item_fn
            .sig
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
                        &current_scope,
                        global_data_copy,
                    ),
                ),
            })
            .collect::<Vec<_>>();

        let return_type = match &fn_info.syn {
            FnInfoSyn::Standalone(item_fn) => match &item_fn.sig.output {
                ReturnType::Default => RustType::Unit,
                ReturnType::Type(_, type_) => parse_types_for_populate_item_definitions(
                    type_,
                    &fn_info.generics,
                    module_path,
                    &current_scope,
                    global_data_copy,
                ),
            },
            FnInfoSyn::Impl(_) => todo!(),
        };

        fn_info.inputs_types = inputs_types;
        fn_info.return_type = return_type;

        // *scope_count += 1;
        // scope_id.push(*scope_count);
        // populate_impl_blocks_items_and_item_def_fields_stmts(
        //     &item_fn.block.stmts,
        //     module,
        //     global_data_copy,
        //     module_path,
        //     global_impl_blocks_simpl,
        //     // scoped_various_definitions,
        //     scope_id,
        // );
        // scope_id.pop();
    }

    for trait_def in &various_definition.trait_definitons {
        // Currently trait defs don't store any info other than the name, so we don't need to do anything
    }
}

fn populate_impl_blocks_items_and_item_def_fields_individual(
    item: &Item,
    module: &mut ModuleData,
    global_data_copy: &GlobalData,
    module_path: &[String],
    global_impl_blocks_simpl: &mut Vec<RustImplBlockSimple>,
    // scoped_various_definitions: &mut Vec<(Vec<usize>, VariousDefintions, Vec<RustImplBlockSimple>)>,
    scope_id: &mut Vec<usize>,
    scope_count: &mut usize,
) {
    let current_scope_id = if scope_id.is_empty() {
        None
    } else {
        Some(scope_id.clone())
    };

    match item {
        Item::Const(item_const) => {
            let const_def =
                module.lookup_const_known_module(&current_scope_id, &item_const.ident.to_string());

            let rust_type = parse_types_for_populate_item_definitions(
                &item_const.ty,
                &Vec::new(),
                module_path,
                &current_scope_id,
                global_data_copy,
            );

            const_def.type_ = rust_type;
        }
        Item::Enum(item_enum) => {
            let item_def = module.lookup_item_def_known_module_assert_not_func2(
                &current_scope_id,
                &item_enum.ident.to_string(),
            );

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
                                &item_def.generics,
                                module_path,
                                &current_scope_id,
                                global_data_copy,
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

            match &mut item_def.struct_or_enum_info {
                StructOrEnumDefitionInfo::Struct(_) => {
                    todo!()
                }
                StructOrEnumDefitionInfo::Enum(enum_def_info) => {
                    enum_def_info.members = members_for_scope;
                }
            }
        }
        Item::ExternCrate(_) => todo!(),
        Item::Fn(item_fn) => {
            let fn_info =
                module.lookup_fn_known_module(&current_scope_id, &item_fn.sig.ident.to_string());

            let inputs_types = item_fn
                .sig
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
                            &current_scope_id,
                            global_data_copy,
                        ),
                    ),
                })
                .collect::<Vec<_>>();

            let return_type = match &item_fn.sig.output {
                ReturnType::Default => RustType::Unit,
                ReturnType::Type(_, type_) => parse_types_for_populate_item_definitions(
                    type_,
                    &fn_info.generics,
                    module_path,
                    &current_scope_id,
                    global_data_copy,
                ),
            };

            fn_info.inputs_types = inputs_types;
            fn_info.return_type = return_type;

            *scope_count += 1;
            scope_id.push(*scope_count);
            populate_impl_blocks_items_and_item_def_fields_stmts(
                &item_fn.block.stmts,
                module,
                global_data_copy,
                module_path,
                global_impl_blocks_simpl,
                // scoped_various_definitions,
                scope_id,
            );
            scope_id.pop();
        }
        Item::ForeignMod(_) => todo!(),
        Item::Impl(item_impl) => {
            // Temporarily store impl block's type params on global data

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

                                        let (module_path, scope_id, trait_def) = global_data_copy
                                            .lookup_trait_definition_any_module(
                                                module_path,
                                                &current_scope_id,
                                                trait_path,
                                            );
                                        Some((module_path, scope_id, trait_def.name))
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

            let trait_path_and_name = item_impl.trait_.as_ref().map(|(_, trait_, _)| {
                let (module_path, trait_scope_id, trait_def) = global_data_copy
                    .lookup_trait_definition_any_module(
                        module_path,
                        &current_scope_id,
                        trait_.segments.iter().map(|seg| seg.ident.to_string()),
                    );
                (module_path, trait_scope_id, trait_def.name)
            });

            let (target_rust_type, is_target_type_param) =
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
                    let (target_item_module, resolved_scope_id, target_item) = global_data_copy
                        .lookup_item_definition_any_module_or_scope(
                            module_path,
                            &current_scope_id,
                            &impl_item_target_path,
                        );
                    // dbg!("here");
                    (
                        RustType::StructOrEnum(
                            target_item
                                .generics
                                .iter()
                                .map(|g| RustTypeParam {
                                    name: g.clone(),
                                    type_: RustTypeParamValue::Unresolved,
                                })
                                .collect::<Vec<_>>(),
                            target_item_module.clone(),
                            resolved_scope_id,
                            target_item.ident.to_string(),
                        ),
                        false,
                    )
                };

            // global_data.impl_block_target_type.pop();

            let rust_items = item_impl
                .items
                .iter()
                .map(|syn_item| {
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
                                    FnArg::Receiver(_) => {
                                        // TODO need to actually parse the reciever to determine if it is boxed or a &mut so we can properly handle derefs
                                        // TODO need to ensure we are clear and consistent with the meaning of `RustType::ParentItem`
                                        (true, false, "self".to_string(), RustType::ParentItem)
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
                                            module_path,
                                            &current_scope_id,
                                            global_data_copy,
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
                                        module_path,
                                        &current_scope_id,
                                        global_data_copy,
                                    )
                                }
                            };

                            *scope_count += 1;
                            scope_id.push(*scope_count);
                            populate_impl_blocks_items_and_item_def_fields_stmts(
                                &impl_item_fn.block.stmts,
                                module,
                                global_data_copy,
                                module_path,
                                global_impl_blocks_simpl,
                                scope_id,
                            );
                            scope_id.pop();

                            let is_pub = match impl_item_fn.vis {
                                Visibility::Public(_) => true,
                                Visibility::Restricted(_) => todo!(),
                                Visibility::Inherited => false,
                            };
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
                                    ident: item_name.clone(),
                                    is_pub,
                                    inputs_types,
                                    generics: fn_generics,
                                    return_type,
                                    syn: FnInfoSyn::Impl(impl_item_fn.clone()),
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

            global_impl_blocks_simpl.push(RustImplBlockSimple {
                unique_id: get_item_impl_unique_id(module_path, &current_scope_id, item_impl),
                generics: rust_impl_block_generics,
                trait_: trait_path_and_name,
                target: target_rust_type.clone(),
                rust_items,
            });
        }
        Item::Macro(_) => {}
        Item::Mod(item_mod) => {
            // Modules should have already been converted to `ModuleData`s
            // TODO Can't assert because these items will still exist even though we now have `ModuleData`s, should clean this up so we can assert or be more confident from simplicity
            // assert!(item_mod.content.is_none());
        }
        Item::Static(_) => todo!(),
        Item::Struct(item_struct) => {
            let item_def = module.lookup_item_def_known_module_assert_not_func2(
                &current_scope_id,
                &item_struct.ident.to_string(),
            );

            let fields = if item_struct.fields.is_empty() {
                StructFieldInfo::UnitStruct
            } else if item_struct.fields.iter().next().unwrap().ident.is_some() {
                StructFieldInfo::RegularStruct(
                    item_struct
                        .fields
                        .iter()
                        .map(|f| {
                            // dbg!(&f.ty);
                            // dbg!(&module_path);
                            // dbg!(&current_scope_id);
                            // dbg!(&scope_id);
                            (
                                f.ident.as_ref().unwrap().to_string(),
                                parse_types_for_populate_item_definitions(
                                    &f.ty,
                                    &item_def.generics,
                                    module_path,
                                    &current_scope_id,
                                    global_data_copy,
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
                                &item_def.generics,
                                module_path,
                                &current_scope_id,
                                global_data_copy,
                            )
                        })
                        .collect::<Vec<_>>(),
                )
            };
            match &mut item_def.struct_or_enum_info {
                StructOrEnumDefitionInfo::Struct(struct_def_info) => {
                    struct_def_info.fields = fields;
                }
                StructOrEnumDefitionInfo::Enum(_) => todo!(),
            }
        }
        Item::Trait(item_trait) => {
            let _item_def =
                module._lookup_trait_known_module(&current_scope_id, &item_trait.ident.to_string());
            // Currently trait defs don't store any info other than the name, so we don't need to do anything
        }
        Item::TraitAlias(_) => todo!(),
        Item::Type(_) => todo!(),
        Item::Union(_) => todo!(),
        Item::Use(_) => {}
        Item::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}
