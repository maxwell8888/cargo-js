use syn::{
    Expr, FnArg, GenericParam, ImplItem, ImplItemFn, Item, ItemConst, ItemEnum, ItemFn, ItemImpl,
    ItemStruct, ItemTrait, Member, Pat, ReturnType, Stmt, Type, TypeParamBound, Visibility,
};
use tracing::debug_span;

use crate::{
    get_item_impl_unique_id, parse_types_for_populate_item_definitions, GlobalData, RustGeneric,
    RustImplBlockSimple, RustImplItemItemNoJs, RustImplItemNoJs, RustType, RustTypeParam,
    RustTypeParamValue,
};

pub fn update_item_definitions(
    global_data_copy: &GlobalData,
    modules: &mut [ModuleData],
) -> Vec<RustImplBlockSimple> {
    // let global_data_copy = global_data.clone();

    let mut global_impl_blocks_simpl = Vec::new();

    for module in modules {
        debug_span!(
            "extract_data_populate_item_definitions module: {:?}",
            module_path = ?module.path
        );
        let module_path = module.path.clone();

        update_various_def(
            &mut module.various_definitions,
            global_data_copy,
            &module_path,
            &None,
        );

        for (scope, various_def) in &mut module.scoped_various_definitions {
            update_various_def(
                various_def,
                global_data_copy,
                &module_path,
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
                                        &module_path,
                                        &(!scope_id.is_empty()).then_some(scope_id.clone()),
                                        global_data_copy,
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
                    item_impl,
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
    global_impl_blocks_simpl
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
    various_definition: &mut VariousDefintions,
    global_data_copy: &GlobalData,
    module_path: &[String],
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
                                    current_scope,
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
                        current_scope,
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
                    current_scope,
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

    for _trait_def in &various_definition.trait_definitons {
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
        Item::Mod(_item_mod) => {
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
    /// (unique impl id)
    pub impl_block_ids: Vec<String>,
}
impl ItemDefinition {
    pub fn get_type(&self, field_member: &Member) -> RustType {
        match &self.struct_or_enum_info {
            StructOrEnumDefitionInfo::Struct(struct_def_info) => match &struct_def_info.fields {
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
            },
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
    /// NOTE the path includes the name of the module, eg the path to the crate module is ["crate"] not [].
    fn new(name: String, parent_name: Option<String>, path: Vec<String>) -> Self {
        ModuleData {
            name,
            // parent_name,
            path,
            // pub_definitions: Vec::new(),
            // private_definitions: Vec::new(),
            pub_submodules: Vec::new(),
            private_submodules: Vec::new(),
            pub_use_mappings: Vec::new(),
            private_use_mappings: Vec::new(),
            resolved_mappings: Vec::new(),
            // fn_info: Vec::new(),
            // item_definitons: Vec::new(),
            // trait_definitons: Vec::new(),
            // consts: Vec::new(),
            various_definitions: VariousDefintions::default(),
            items: Vec::new(),
            scoped_various_definitions: Vec::new(),
            scoped_syn_impl_items: Vec::new(),
        }
    }
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

    pub fn lookup_item_def_known_module_assert_not_func2(
        &mut self,
        scope_id: &Option<Vec<usize>>,
        name: &str,
    ) -> &mut ItemDefinition {
        let scoped_item_def = scope_id
            .as_ref()
            .and_then(|scope_id| {
                self.scoped_various_definitions
                    .iter_mut()
                    .find(|svd| &svd.0 == scope_id)
            })
            .and_then(|svd| {
                svd.1
                    .item_definitons
                    .iter_mut()
                    .find(|item_def| item_def.ident == name)
            });
        let module_item_def = self
            .various_definitions
            .item_definitons
            .iter_mut()
            .find(|item_def| item_def.ident == name);

        scoped_item_def.or(module_item_def).unwrap()
    }

    pub fn lookup_const_known_module(
        &mut self,
        scope_id: &Option<Vec<usize>>,
        name: &str,
    ) -> &mut ConstDef {
        let scoped_const_def = scope_id
            .as_ref()
            .and_then(|scope_id| {
                self.scoped_various_definitions
                    .iter_mut()
                    .find(|svd| &svd.0 == scope_id)
            })
            .and_then(|svd| {
                svd.1
                    .consts
                    .iter_mut()
                    .find(|const_def| const_def.name == name)
            });
        let module_const_def = self
            .various_definitions
            .consts
            .iter_mut()
            .find(|const_def| const_def.name == name);

        scoped_const_def.or(module_const_def).unwrap()
    }

    pub fn lookup_fn_known_module(
        &mut self,
        scope_id: &Option<Vec<usize>>,
        name: &str,
    ) -> &mut FnInfo {
        let scoped_fn_info = scope_id
            .as_ref()
            .and_then(|scope_id| {
                self.scoped_various_definitions
                    .iter_mut()
                    .find(|svd| &svd.0 == scope_id)
            })
            .and_then(|svd| {
                svd.1
                    .fn_info
                    .iter_mut()
                    .find(|const_def| const_def.ident == name)
            });
        let module_fn_info = self
            .various_definitions
            .fn_info
            .iter_mut()
            .find(|const_def| const_def.ident == name);

        scoped_fn_info.or(module_fn_info).unwrap()
    }

    pub fn _lookup_trait_known_module(
        &mut self,
        scope_id: &Option<Vec<usize>>,
        name: &str,
    ) -> &mut RustTraitDefinition {
        let scoped_trait_def = scope_id
            .as_ref()
            .and_then(|scope_id| {
                self.scoped_various_definitions
                    .iter_mut()
                    .find(|svd| &svd.0 == scope_id)
            })
            .and_then(|svd| {
                svd.1
                    .trait_definitons
                    .iter_mut()
                    .find(|trait_def| trait_def.name == name)
            });
        let module_trait_def = self
            .various_definitions
            .trait_definitons
            .iter_mut()
            .find(|trait_def| trait_def.name == name);

        scoped_trait_def.or(module_trait_def).unwrap()
    }
}

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
