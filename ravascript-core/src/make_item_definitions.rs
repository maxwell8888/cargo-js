use syn::{
    Expr, GenericParam, ImplItem, ImplItemFn, Item, ItemConst, ItemEnum, ItemFn, ItemImpl,
    ItemStruct, ItemTrait, Meta, Signature, Stmt, Type, Visibility,
};
use tracing::{debug, debug_span};

use crate::{
    extract_modules::ModuleDataFirstPass,
    tree_structure::{ItemActual, ItemRef, RustMod, StmtsRef},
    RustPathSegment, PRELUDE_MODULE_PATH,
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
            various_definitions: VariousDefintions::default(),
            items: module_first_pass.items,
            scoped_various_definitions: Vec::new(),
            syn_impl_items: Vec::new(),
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
                struct_or_enum_info: StructOrEnumDefitionInfo::Enum(item_enum.clone()),
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
                generics,
                // return_type: RustType::Uninit,
                syn: FnInfoSyn::Standalone(item_fn.clone()),
                signature: item_fn.sig.clone(),
                stmts: Vec::new(),
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
            // There is no reason to create a version of `RustImplBlockSimple` without RustType's here. We do for the other items, because we need the names of the types to be able to create the `RustType`s for fields etc, but impl blocks don't create anything that lives in `RustType`s? So given the `RustImplBlockSimple` itself *does* need to use `RustType`s we need to create it in `update_item_definitions()` As a matter of fact, there is not much point doing anything here other than recording the names, and then doing all the syn parsing in update_item_definitions, rather than having this somewhat confusing artificial separation.

            // TODO IMPORTANT currently we are adding top level impl blocks to `global_data.impl_blocks` in handle_item_impl(). It would be better to push (non-scoped) impl blocks here, so that they are already available if a method defined on the impl is called before the impl block itself is reached/parsed by `handle_item_impl()`. However we still need to find a way to solve this problem for the scoped impl blocks anyway. Leave it as is for now until we do some refactoring and deduplication, to avoid need to repeat a bunch of code here.

            module
                .syn_impl_items
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
            various_defs.item_definitons.push(ItemDefinition {
                ident: item_struct.ident.to_string(),
                is_pub,
                is_copy,
                generics,
                struct_or_enum_info: StructOrEnumDefitionInfo::Struct(item_struct.clone()),
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
    Struct(ItemStruct),
    Enum(ItemEnum),
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
    pub syn_impl_items: Vec<(Vec<usize>, ItemImpl)>,
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

/// Not just for methods, can also be an enum variant with no inputs
#[derive(Debug, Clone)]
pub struct FnInfo {
    pub ident: String,
    pub is_pub: bool,
    pub generics: Vec<String>,
    // TODO remove this, just legacy thing we need for now because it gets used in the JS parsing (I think)
    pub syn: FnInfoSyn,

    pub signature: Signature,
    // pub syn: FnInfoSyn,
    pub stmts: Vec<StmtsRef>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum FnInfoSyn {
    Standalone(ItemFn),
    Impl(ImplItemFn),
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

#[allow(clippy::too_many_arguments)]
pub fn resolve_path(
    // look_for_scoped_vars: bool,
    // TODO can we combine this with `look_for_scoped_vars`?
    look_for_scoped_items: bool,
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
