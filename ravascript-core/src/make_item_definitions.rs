use syn::{
    Expr, GenericParam, ImplItem, ImplItemFn, Item, ItemConst, ItemEnum, ItemFn, ItemImpl,
    ItemStruct, ItemTrait, Member, Meta, Stmt, Type, Visibility,
};
use tracing::debug_span;

use crate::{
    extract_modules::ModuleDataFirstPass,
    update_item_definitions::{
        ConstDef, EnumDefinitionInfo, EnumVariantInfo, FnInfo, FnInfoSyn, ItemDefinition,
        ModuleData, RustTraitDefinition, StructDefinitionInfo, StructFieldInfo,
        StructOrEnumDefitionInfo, VariousDefintions,
    },
    RustType, RustTypeParam, RustTypeParamValue,
};

pub fn make_item_definitions(modules: Vec<ModuleDataFirstPass>) -> Vec<ModuleData> {
    // TODO the code for eg module.item_definitions.push(...) is a duplicated also for scope.item_definitons.push(...). Remove this duplication.

    // This is because parse_types_for_populate_item_definitions needs a access to .pub_definitions etc in global_data from `extract_data()` but we are taking an immutable ref first
    // We also need it for looking up trait definitions

    let mut new_modules = Vec::new();
    for module_first_pass in modules {
        debug_span!(
            "extract_data_populate_item_definitions module: {:?}",
            module_path = ?module_first_pass.path
        );

        let mut module = ModuleData {
            name: module_first_pass.name,
            // parent_name: module,
            path: module_first_pass.path,
            pub_submodules: module_first_pass.pub_submodules,
            private_submodules: module_first_pass.private_submodules,
            pub_use_mappings: module_first_pass.pub_use_mappings,
            private_use_mappings: module_first_pass.private_use_mappings,
            resolved_mappings: Vec::new(),
            // fn_info: Vec::new(),
            // item_definitons: Vec::new(),
            // consts: Vec::new(),
            // trait_definitons: Vec::new(),
            various_definitions: VariousDefintions::default(),
            items: module_first_pass.items,
            scoped_various_definitions: Vec::new(),
            scoped_syn_impl_items: Vec::new(),
        };

        // TODO Gymnastics to reconcile needing to mutate 4 different vectors which are stored differently for modules and scopes. Should probably have `module.various_defs` and `scope.various_defs` fields
        let mut var_defs = VariousDefintions::default();
        let items = module.items.clone();
        let module_path = module.path.clone();
        let mut scope_id = Vec::new();

        let mut scope_count = 0;
        for item in &items {
            populate_item_definitions_items_individual_item(
                item,
                &module_path,
                &mut var_defs,
                &mut module,
                &mut scope_id,
                &mut scope_count,
            )
        }
        // module.various_definitions.fn_info.extend(var_defs.fn_info);
        // module
        //     .various_definitions
        //     .item_definitons
        //     .extend(var_defs.item_definitons);
        // module.various_definitions.consts.extend(var_defs.consts);
        // module
        //     .various_definitions
        //     .trait_definitons
        //     .extend(var_defs.trait_definitons);
        module.various_definitions = var_defs;
        new_modules.push(module);
    }
    new_modules
}

fn populate_item_definitions_items_individual_item(
    item: &Item,
    // global_data: &GlobalData,
    module_path: &[String],
    // These `various_defs` are will either be added to a module if this fn is called when iterating over module level items, or a scope is it is called when iterating over stmts
    various_defs: &mut VariousDefintions,
    module: &mut ModuleData,
    scope_id: &mut Vec<usize>,
    scope_count: &mut usize,
) {
    match item {
        Item::Const(item_const) => {
            let const_name = item_const.ident.to_string();

            let is_pub = match item_const.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };
            various_defs.consts.push(ConstDef {
                name: const_name,
                is_pub,
                type_: RustType::Todo,
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
                    inputs: Vec::new(),
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
            various_defs.item_definitons.push(ItemDefinition {
                ident: enum_name,
                is_copy,
                is_pub,
                generics,
                struct_or_enum_info: StructOrEnumDefitionInfo::Enum(EnumDefinitionInfo {
                    members: members_for_scope,
                    syn_object: item_enum.clone(),
                }),
                impl_block_ids: Vec::new(),
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

            let is_pub = match item_fn.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };
            various_defs.fn_info.push(FnInfo {
                ident: item_fn.sig.ident.to_string(),
                is_pub,
                inputs_types: Vec::new(),
                generics,
                // return_type: RustType::Uninit,
                return_type: RustType::Todo,
                syn: FnInfoSyn::Standalone(item_fn.clone()),
            });

            // Get scoped definitions

            // dbg!("populate_item_definitions_items");
            // let sig = &item_fn.sig;
            // println!("{}", quote! { #sig });
            // dbg!(scope_count);

            // let mut itemms = Vec::new();
            // for stmt in item_fn.block.stmts.clone().into_iter() {
            //     append_items_from_stmt(&mut itemms, stmt);
            // }

            // *scope_number.last_mut().unwrap() += 1;

            // dbg!("push item_fn scope");
            // let sig = &item_fn.sig;
            // println!("{}", quote! { #item_fn });
            // dbg!(&scope_count);
            // dbg!(&scope_id);
            *scope_count += 1;
            let mut scoped_various_defs = VariousDefintions::default();

            // We are now processing the items within the fn block, so are no longer at the module level and now in a scope (or in a new child scope), so push a new scope level
            scope_id.push(*scope_count);
            // dbg!(&scope_count);
            // dbg!(&scope_id);
            // dbg!("populate item fn");
            // dbg!(item_fn.sig.ident.to_string());
            // dbg!(&scope_id);
            // populate_item_definitions_items(
            //     &itemms,
            //     global_data,
            //     module_path,
            //     &mut scoped_various_defs,
            //     module,
            //     scope_id,
            // );
            populate_item_definitions_stmts(
                &item_fn.block.stmts,
                // global_data,
                module_path,
                &mut scoped_various_defs,
                module,
                scope_id,
            );

            // TODO it seems like it would be more simple and intuitive to create the `VariousDefinitions` scope and add it to the `module.scoped_various_definitions` in the `populate_item_definitions_stmts()` fn rather than doing it outside like this. Will wait until we have all tests passing before attempting to make this change.
            module.scoped_various_definitions.push((
                scope_id.clone(),
                scoped_various_defs,
                // Vec::new(),
            ));
            // dbg!("pop item_fn scope");
            scope_id.pop();
            // dbg!(&scope_count);
            // dbg!(&scope_id);
        }
        Item::ForeignMod(_) => todo!(),
        Item::Impl(item_impl) => {
            // TODO IMPORTANT currently we are adding top level impl blocks to `global_data.impl_blocks` in handle_item_impl(). It would be better to push (non-scoped) impl blocks here, so that they are already available if a method defined on the impl is called before the impl block itself is reached/parsed by `handle_item_impl()`. However we still need to find a way to solve this problem for the scoped impl blocks anyway. Leave it as is for now until we do some refactoring and deduplication, to avoid need to repeat a bunch of code here.

            module
                .scoped_syn_impl_items
                .push((scope_id.clone(), item_impl.clone()));

            // TODO also need to go through scopes in impl fns, like with standalone fns
            for item in &item_impl.items {
                match item {
                    ImplItem::Const(_) => todo!(),
                    ImplItem::Fn(impl_item_fn) => {
                        // dbg!("push item_impl_fn scope");
                        // dbg!(module_path);
                        // let sig = &impl_item_fn.sig;
                        // println!("{}", quote! { #impl_item_fn });
                        // dbg!(&scope_count);
                        // dbg!(&scope_id);
                        *scope_count += 1;
                        scope_id.push(*scope_count);
                        // dbg!(&scope_count);
                        // dbg!(&scope_id);
                        let mut scoped_various_defs = VariousDefintions::default();
                        populate_item_definitions_stmts(
                            &impl_item_fn.block.stmts,
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
                        // dbg!("pop item_impl_fn scope");
                        scope_id.pop();
                        // dbg!(&scope_count);
                        // dbg!(&scope_id);
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
        Item::Mod(_) => {}
        Item::Static(_) => todo!(),
        Item::Struct(item_struct) => {
            // dbg!("populate_item_definitions_items_individual_item");
            // println!("{}", quote! { #item_struct });
            let struct_name = item_struct.ident.to_string();

            // Make ItemDefinition
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

            let generics_type_params = generics
                .iter()
                .map(|name| RustTypeParam {
                    name: name.clone(),
                    type_: RustTypeParamValue::Unresolved,
                })
                .collect::<Vec<_>>();

            let fields = if item_struct.fields.is_empty() {
                StructFieldInfo::UnitStruct
            } else if item_struct.fields.iter().next().unwrap().ident.is_some() {
                StructFieldInfo::RegularStruct(Vec::new())
            } else {
                StructFieldInfo::TupleStruct(Vec::new())
            };

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
            various_defs.item_definitons.push(ItemDefinition {
                ident: item_struct.ident.to_string(),
                is_pub,
                is_copy,
                generics,
                struct_or_enum_info: StructOrEnumDefitionInfo::Struct(StructDefinitionInfo {
                    fields,
                    syn_object: item_struct.clone(),
                }),
                impl_block_ids: Vec::new(),
            });
        }
        Item::Trait(item_trait) => {
            let is_pub = match item_trait.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            };
            various_defs.trait_definitons.push(RustTraitDefinition {
                name: item_trait.ident.to_string(),
                is_pub,
                syn: item_trait.clone(),
            })
        }
        Item::TraitAlias(_) => todo!(),
        Item::Type(_) => todo!(),
        Item::Union(_) => todo!(),
        Item::Use(_) => {}
        Item::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}

fn populate_item_definitions_stmts(
    stmts: &[Stmt],
    // global_data: &GlobalData,
    module_path: &[String],
    current_scope_various_defs: &mut VariousDefintions,
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
            Stmt::Item(item) => populate_item_definitions_items_individual_item(
                item,
                // global_data,
                module_path,
                current_scope_various_defs,
                module,
                scope_id,
                &mut scope_count,
            ),
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
