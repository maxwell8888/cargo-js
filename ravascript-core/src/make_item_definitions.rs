use std::path::PathBuf;

use syn::{
    Expr, GenericParam, ImplItem, ImplItemFn, Item, ItemConst, ItemEnum, ItemFn, ItemImpl,
    ItemStruct, ItemTrait, Member, Meta, PathArguments, Stmt, Type, Visibility,
};
use tracing::{debug, debug_span};

use crate::{
    duplicate_namespacing::Duplicate, extract_modules::ModuleDataFirstPass, js_ast::JsModule,
    CrateData, GlobalDataScope, JsImplBlock2, JsImplItem, RustImplBlockSimple,
    RustImplItemItemNoJs, RustImplItemNoJs, RustPathSegment, RustPreludeTypes, RustType,
    RustTypeFnType, RustTypeParam, RustTypeParamValue, ScopedVar, PRELUDE_MODULE_PATH,
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

// TODO clean up redundant code
#[derive(Debug, Clone)]
pub struct GlobalData {
    crate_path: Option<PathBuf>,
    pub modules: Vec<ModuleData>,
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
    /// Similar to impl_block_target_type but if for storing type params of the impl eg `impl<A, B> Foo<A, B> { ... }` so that when `A` and `B` appears in one of the impl's item definitions and we try and lookup the path `A` and `B` with `resolve_path()` we can also look here to find the type params.
    /// TODO Should be Vec of Vecs for same reason impl_block_target_type is a Vec
    impl_block_type_params: Vec<RustTypeParam>,
    // TODO handle closures - which don't have explicitly specified return type, need to infer it from return value
    // scoped_fns: Vec<ItemFn>,
    rust_prelude_types: RustPreludeTypes,
    // TODO why have this seprate to module.item_def? Because they aren't defined anywhere and are available in all modules so don't really belong to a module?
    // (rust name, js primative type name, item definition)
    // TODO why store rust name when it is in ItemDefinition???
    // TODO should this just be a big struct rather than a vec so we don't have to do lookups?
    // rust_prelude_definitions: Vec<(String, String, ItemDefinition)>,
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
    // We keep the impl blocks at the crate level rather than in the relevant Module because different it is not possible to impl the same eg method name on the same struct, even using impl blocks in completely separate modules. Impl item idents must be unique for a given type across the entire crate. I believe this is also the case for scoped impls? This is because impl'd items are available on the item definition/instance they are targetting, not only in parent scopes, but also parent modules.
    // impl_blocks: Vec<ItemImpl>,
    impl_blocks: Vec<JsImplBlock2>,
    /// The purpose of having this here is so that all crate scoped impl blocks are immeditately available when parsing the syn to JS, eg if we come across a class (module level or scoped), we want to be able to add the methods which are implemented for it at that point, but these impls might not appear until later in the code, so instead we popualte scoped_impl_blocks in extract_data_populate_item_definitions to ensure it is available
    /// Given method names (impld for the same item) must be unqiue across the crate, for module level impls, we can just store all impl blocks in a big list, and add all their methods to all matching classes/items/enums/structs, whether the method is private/public is irrelevant since if it has been defined it must/should get used at some point.
    /// Scoped impls are a litte more complicated though, because in the same way we distinguish between different module level structs with the same name by taking into account their module path, for scoped structs we need to take into account the scope, ie a scoped `impl Foo { ... }` should only be applied to the first `Foo` that is found in parent scopes, else any module (of course taking into account the full module path used in `impl Foo { ... }`), because there might be another `Foo` in a higher scope with the same method impld, so we must not apply it there.
    /// We don't have to
    /// ((module path, scope id), rust impl block))
    #[allow(clippy::type_complexity)]
    scoped_impl_blocks: Vec<((Vec<String>, Vec<usize>), JsImplBlock2)>,
    /// Testing: for the purpose of populating `item_definition.impl_items` see if we can store less info about impl blocks. We need the "signature" to be parsed so that we can easily determine whether the target is a type param or concrete type (or mixture - TODO), and also id's for the traits involved, ie the bounds on generics and the trait being impl.
    impl_blocks_simpl: Vec<RustImplBlockSimple>,
    duplicates: Vec<Duplicate>,
    transpiled_modules: Vec<JsModule>,
    // /// For keeping track of whether we are parsing items at the module level or in a fn scope, so that we know whether we need to add the items to `.scopes` or not.
    // at_module_top_level: bool,
    // 1 based
    scope_id: Vec<usize>,
    // 1 based
    scope_count: Vec<usize>,
}
impl GlobalData {
    pub fn new(crate_path: Option<PathBuf>) -> GlobalData {
        // let option_def = ItemDefinition {
        //     ident: "Option".to_string(),
        //     is_copy: false,
        //     generics: vec!["T".to_string()],
        //     struct_or_enum_info: StructOrEnumDefitionInfo::Enum(EnumDefinitionInfo {
        //         members: vec![
        //             EnumVariantInfo {
        //                 ident: "Some".to_string(),
        //                 inputs: vec![EnumVariantInputsInfo::Unnamed(RustType::TypeParam(
        //                     RustTypeParam {
        //                         name: "T".to_string(),
        //                         type_: RustTypeParamValue::Unresolved,
        //                     },
        //                 ))],
        //             },
        //             EnumVariantInfo {
        //                 ident: "None".to_string(),
        //                 inputs: Vec::new(),
        //             },
        //         ],
        //     }),
        //     impl_block_ids: Vec::new(),
        // };

        // let ravascript_prelude_crate = CrateData {
        //     name: "ravascript".to_string(),
        // };

        // let mut impl_blocks_simpl = Vec::new();
        // let mut rust_items = Vec::new();
        // rust_items.push(RustImplItemNoJs {
        //     ident: "is_some_and".to_string(),
        //     item: RustImplItemItemNoJs::Fn(
        //         false,
        //         false,
        //         FnInfo {
        //             ident: "is_some_and".to_string(),
        //             inputs_types: vec![RustType::Closure(
        //                 vec![RustType::TypeParam()],
        //                 RustType::Bool,
        //             )],
        //             generics: (),
        //             return_type: (),
        //         },
        //     ),
        //     syn_object: (),
        // });
        // impl_blocks_simpl.push(RustImplBlockSimple {
        //     unique_id: "is this needed?".to_string(),
        //     generics: vec![RustGeneric {
        //         ident: "T".to_string(),
        //         trait_bounds: Vec::new(),
        //     }],
        //     trait_: None,
        //     target: RustType::Option(Box::new(RustType::TypeParam(RustTypeParam {
        //         name: "T".to_string(),
        //         type_: RustTypeParamValue::Unresolved,
        //     }))),
        //     rust_items,
        //     // TODO do we need this??
        //     items: Vec::new(),
        // });

        // let code = include_str!("rust_prelude/option.rs");
        // let modules = from_file(code, false);
        // assert_eq!(modules.len(), 1);
        // let option_module = &modules[0];

        // for stmt in &option_module.stmts {
        //     match stmt {
        //         JsStmt::Class(js_class) => {
        //             if js_class.name == "Option" {
        //                 prelude_stmts.push(stmt.clone());
        //             }
        //         }
        //         JsStmt::ClassMethod(_, _, _, _) => todo!(),
        //         JsStmt::ClassStatic(_) => todo!(),
        //         // JsStmt::Local(js_local) => match &js_local.lhs {
        //         //     LocalName::Single(name) => {
        //         //         if name == "Some" || name == "None" {
        //         //             js_stmts.insert(0, stmt.clone());
        //         //         }
        //         //     }
        //         //     LocalName::DestructureObject(_) => todo!(),
        //         //     LocalName::DestructureArray(_) => todo!(),
        //         // },
        //         JsStmt::Local(_) => todo!(),
        //         JsStmt::Expr(_, _) => todo!(),
        //         JsStmt::Import(_, _, _) => todo!(),
        //         JsStmt::Function(_) => todo!(),
        //         JsStmt::ScopeBlock(_) => todo!(),
        //         // JsStmt::TryBlock(_) => todo!(),
        //         // JsStmt::CatchBlock(_, _) => todo!(),
        //         JsStmt::Raw(_) => todo!(),
        //         JsStmt::Comment(_) => todo!(),
        //     }
        // }

        GlobalData {
            crate_path,
            modules: Vec::new(),
            // crates: vec![ravascript_prelude_crate],
            crates: vec![],
            // init with an empty scope to ensure `scopes.last()` always returns something TODO improve this
            scopes: vec![GlobalDataScope {
                scope_id: Vec::new(),
                variables: Vec::new(),
                look_in_outer_scope: false,
                use_mappings: Vec::new(),
            }],
            // struct_or_enum_methods: Vec::new(),
            impl_block_target_type: Vec::new(),
            impl_block_type_params: Vec::new(),
            // scoped_fns: vec![],
            rust_prelude_types: RustPreludeTypes::default(),
            default_trait_impls_class_mapping: Vec::new(),
            default_trait_impls: Vec::new(),
            // impl_items_for_js: Vec::new(),
            duplicates: Vec::new(),
            transpiled_modules: Vec::new(),
            impl_blocks: Vec::new(),
            scoped_impl_blocks: Vec::new(),
            impl_blocks_simpl: Vec::new(),
            scope_id: Vec::new(),
            scope_count: vec![0],
            // at_module_top_level: false,
        }
    }

    fn scope_id_as_option(&self) -> Option<Vec<usize>> {
        if self.scope_id.is_empty() {
            None
        } else {
            Some(self.scope_id.clone())
        }
    }

    fn push_new_scope(&mut self, look_in_outer_scope: bool, variables: Vec<ScopedVar>) {
        let scope_count = {
            let scope_count = self.scope_count.last_mut().unwrap();
            *scope_count += 1;
            *scope_count
        };
        self.scope_id.push(scope_count);
        let var_scope = GlobalDataScope {
            scope_id: self.scope_id.clone(),
            variables,
            look_in_outer_scope,
            use_mappings: Vec::new(),
        };
        self.scopes.push(var_scope);
        self.scope_count.push(0);
    }
    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.scope_count.pop();
        self.scope_id.pop();
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
    // fn lookup_scoped_var_or_item_definiton(&self, path: &Vec<String>) -> Option<VarItemFn> {
    //     self.scopes.iter().rev().find_map(|scope| {
    //         // Note variables, and items definitions can't shadow each other in the same scope, so don't need to worry about the order in which each var and item definition was defined in the scope, ie vars and item definitions don't need to be together sorted in one big list, can't just look through the list of vars *then* look through the list of item definitions.
    //         let first = &path[0];
    //         let var = scope.variables.iter().find(|v| &v.name == first);
    //         let func = scope.fns.iter().find(|f| &f.ident == first);
    //         let se = scope.item_definitons.iter().find(|se| &se.ident == first);
    //         if path.len() == 1 {
    //             if let Some(var) = var {
    //                 return Some(VarItemFn::Var(var.clone()));
    //             } else if let Some(func) = func {
    //                 return Some(VarItemFn::Fn(func.clone()));
    //             } else if let Some(se) = se {
    //                 return Some(VarItemFn::StructOrEnum(se.clone()));
    //             } else {
    //                 return None;
    //             }
    //         } else if path.len() == 2 {
    //             // if let Some(var) = var {
    //             //     return Some(VarItemFn::Var(var.clone()));
    //             // } else if let Some(func) = func {
    //             //     return Some(VarItemFn::Fn(func.clone()));
    //             // } else if let Some(se) = se {
    //             //     return Some(VarItemFn::StructOrEnum(se.clone()));
    //             // } else {
    //             //     return None;
    //             // }
    //             todo!()
    //         } else {
    //             todo!()
    //         }
    //     })
    // }

    // fn lookup_fn_definition_known_module(
    //     &self,
    //     name: String,
    //     module_path: &Option<Vec<String>>,
    // ) -> FnInfo {
    //     if let Some(module_path) = module_path {
    //         let module = self
    //             .modules
    //             .iter()
    //             .find(|m| &m.path == module_path)
    //             .unwrap();
    //         module
    //             .fn_info
    //             .iter()
    //             .cloned()
    //             .find(|se| se.ident == name)
    //             .unwrap()
    //     } else {
    //         self.scopes
    //             .iter()
    //             .rev()
    //             .find_map(|s| s.fns.iter().rev().cloned().find(|se| se.ident == name))
    //             .unwrap()
    //     }
    // }

    // fn lookup_item_definition_known_module(
    //     &self,
    //     name: &String,
    //     module_path: &Option<Vec<String>>,
    // ) -> ItemDefinition {
    //     if let Some(module_path) = module_path {
    //         let module = self
    //             .modules
    //             .iter()
    //             .find(|m| &m.path == module_path)
    //             .unwrap();
    //         module
    //             .item_definitons
    //             .iter()
    //             .cloned()
    //             .find(|se| &se.ident == name)
    //             .unwrap()
    //     } else {
    //         self.scopes
    //             .iter()
    //             .rev()
    //             .find_map(|s| {
    //                 s.item_definitons
    //                     .iter()
    //                     .rev()
    //                     .cloned()
    //                     .find(|se| &se.ident == name)
    //             })
    //             .unwrap()
    //     }
    // }

    fn syn_type_to_rust_type_struct_or_enum(
        &self,
        current_module: &[String],
        // generics: &Vec<RustTypeParam>,
        syn_type: &Type,
    ) -> (Vec<RustTypeParam>, Vec<String>, Option<Vec<usize>>, String) {
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
                                PathArguments::AngleBracketed(_) => {
                                    // TODO this is a hack, needs handling properly
                                    Vec::new()
                                }
                                PathArguments::Parenthesized(_) => todo!(),
                            },
                        }
                    })
                    .collect::<Vec<_>>()
            }
            _ => todo!(),
        };

        let (module_path, item_path, item_scope_id) = resolve_path(
            false,
            false,
            true,
            type_path,
            self,
            current_module,
            current_module,
            &self.scope_id_as_option(),
        );
        assert!(item_path.len() == 1);
        // dbg!("yes");
        let item_def = self.lookup_item_def_known_module_assert_not_func2(
            &module_path,
            &item_scope_id,
            &item_path[0].ident,
        );
        // dbg!("ytes222");
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
            item_scope_id,
            item_path[0].ident.clone(),
        )
    }

    // fn lookup_item_def_known_module_assert_not_func(
    //     &self,
    //     module_path: &Option<Vec<String>>,
    //     name: &String,
    // ) -> ItemDefinition {
    //     if let Some(module_path) = module_path {
    //         let module = self
    //             .modules
    //             .iter()
    //             .find(|m| &m.path == module_path)
    //             .unwrap();

    //         let func = module.fn_info.iter().find(|se| &se.ident == name);
    //         assert!(func.is_none());

    //         module
    //             .item_definitons
    //             .iter()
    //             .find(|se| &se.ident == name)
    //             .unwrap()
    //             .clone()
    //     } else {
    //         // Look for scoped items
    //         // dbg!(&self.scopes);
    //         self.scopes
    //             .iter()
    //             .rev()
    //             .find_map(|scope| {
    //                 let var = scope.variables.iter().find(|v| &v.name == name);
    //                 let func = scope.fns.iter().find(|f| &f.ident == name);
    //                 assert!(var.is_none() && func.is_none());

    //                 scope.item_definitons.iter().find(|f| &f.ident == name)
    //             })
    //             .unwrap()
    //             .clone()
    //     }
    // }

    fn lookup_fn_info_known_module(
        &self,
        module_path: &[String],
        scope_id: &Option<Vec<usize>>,
        name: &str,
    ) -> FnInfo {
        let module = self.modules.iter().find(|m| m.path == module_path).unwrap();
        let scoped_fn_info = scope_id
            .as_ref()
            .and_then(|scope_id| {
                module
                    .scoped_various_definitions
                    .iter()
                    .find(|svd| &svd.0 == scope_id)
            })
            .and_then(|svd| svd.1.fn_info.iter().find(|fn_info| fn_info.ident == name));
        let module_fn_info = module
            .various_definitions
            .fn_info
            .iter()
            .find(|fn_info| fn_info.ident == name);

        scoped_fn_info.or(module_fn_info).unwrap().clone()
    }

    fn lookup_item_def_known_module_assert_not_func2(
        &self,
        module_path: &[String],
        scope_id: &Option<Vec<usize>>,
        name: &str,
    ) -> ItemDefinition {
        let module = self.modules.iter().find(|m| m.path == module_path).unwrap();
        let scoped_item_def = scope_id
            .as_ref()
            .and_then(|scope_id| {
                module
                    .scoped_various_definitions
                    .iter()
                    .find(|svd| &svd.0 == scope_id)
            })
            .and_then(|svd| {
                svd.1
                    .item_definitons
                    .iter()
                    .find(|item_def| item_def.ident == name)
            });
        let module_item_def = module
            .various_definitions
            .item_definitons
            .iter()
            .find(|item_def| item_def.ident == name);

        // Dont't need to look for prelude items here since resolve_path already returns a ["prelude_special_case"] module for prelude types. This seems like a better place though, since then we wouldn't need a special module name - well resolve_path still needs to return something? maybe [""] instead?
        // let prelude_item_def = self
        //     .rust_prelude_definitions
        //     .iter()
        //     .find_map(|(_name, _js_name, item_def)| (&item_def.ident == name).then_some(item_def));

        // Might want to check/assert these or useful for debugging
        let is_box_prelude =
            module_path == [PRELUDE_MODULE_PATH] && scope_id.is_none() && name == "Box";

        // if let Some(item_def) = scoped_item_def.or(module_item_def).or(prelude_item_def) {
        if let Some(item_def) = scoped_item_def.or(module_item_def) {
            item_def.clone()
        } else {
            dbg!("could find item def for lookup_item_def_known_module_assert_not_func2");
            dbg!(&module_path);
            dbg!(&scope_id);
            dbg!(&name);
            panic!();
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
    // fn lookup_item_definition_any_module(
    //     &self,
    //     current_module_path: &Vec<String>,
    //     path: &Vec<String>,
    //     // current_module: &Vec<String>,
    // ) -> Option<(Option<Vec<String>>, ItemDefinition)> {
    //     let first = &path[0];
    //     // TODO should just use get_path to look for scoped items?
    //     if let Some(item_def) = self.lookup_scoped_item_definiton(first) {
    //         return Some((None, item_def));
    //     } else {
    //         // dbg!(path);
    //         // dbg!(current_module_path);
    //         let (item_module_path, item_path, _is_scoped) = get_path(
    //             false,
    //             false,
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
    //             &None,
    //         );
    //         // dbg!(&item_module_path);
    //         // dbg!(&item_path);

    //         if item_module_path == vec!["prelude_special_case".to_string()] {
    //             // Get prelude item definitions
    //             let (_name, def) = self
    //                 .rust_prelude_definitions
    //                 .iter()
    //                 .find(|(name, def)| name == &item_path[0].ident)
    //                 .unwrap();
    //             return Some((Some(item_module_path), def.clone()));
    //         }

    //         let item_module = self
    //             .modules
    //             .iter()
    //             .find(|m| &m.path == &item_module_path)
    //             .unwrap();
    //         // dbg!(&item_module);
    //         // dbg!(&item_path[0].ident);

    //         // TODO if the path is eg an associated fn, should we return the item or the fn? ie se or RustType?
    //         let item_def = item_module
    //             .item_definitons
    //             .iter()
    //             .find(|se| se.ident == item_path[0].ident);

    //         if let Some(item_def) = item_def {
    //             return Some((Some(item_module_path), item_def.clone()));
    //         } else {
    //             todo!()
    //         }

    //         // let item_definition = self.
    //         // let module_item_definition = current_module.item_definitons.iter().find(|se| &se.ident == path);
    //         // if let Some(item_def) = scoped_item_definition {
    //         //     // todo!();
    //         //     Some((None, item_def.clone()))
    //         // } else if let Some(item_def) = module_item_definition {
    //         //     // todo!();
    //         //     Some((Some(current_module), item_def.clone()))
    //         // } else {
    //         //     None
    //         // }
    //     }
    // }

    /// NOTE to be used pre syn -> JS parsing, ie self.scopes won't have been populated
    // -> (module path, scope id (even if we are in a scope so `Some` scope id is provided, the item being looked up might still be module level. Of course if None scope id is provided it is impossible for a Some scope id to be returned), item definition)
    pub fn lookup_item_definition_any_module_or_scope(
        &self,
        current_module_path: &[String],
        scope_id: &Option<Vec<usize>>,
        path: &[String],
    ) -> (Vec<String>, Option<Vec<usize>>, ItemDefinition) {
        // dbg!(&path);
        // dbg!(&current_module_path);
        // dbg!(&scope_id);
        // dbg!("get_path");
        let (item_module_path, item_path, item_scope) = resolve_path(
            false,
            true,
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
            scope_id,
        );
        // dbg!(&item_module_path);
        // dbg!(&item_path);
        // dbg!(&item_scope);

        let item_module = self
            .modules
            .iter()
            .find(|m| m.path == item_module_path)
            .unwrap();

        // TODO if the path is eg an associated fn, should we return the item or the fn? ie se or RustType?
        let item_defintions = if let Some(scope_id) = &item_scope {
            &item_module
                .scoped_various_definitions
                .iter()
                .find(|svd| &svd.0 == scope_id)
                .unwrap()
                .1
                .item_definitons
        } else {
            &item_module.various_definitions.item_definitons
        };
        let item_def = item_defintions
            .iter()
            .find(|se| se.ident == item_path[0].ident)
            .unwrap();

        (item_module_path, item_scope, item_def.clone())
    }

    // TODO should also look up fns?
    // fn lookup_scoped_item_definiton(&self, name: &String) -> Option<ItemDefinition> {
    //     self.scopes
    //         .iter()
    //         .rev()
    //         .find_map(|s| s.item_definitons.iter().rev().find(|se| &se.ident == name))
    //         .cloned()
    // }

    pub fn lookup_trait_definition_any_module<I>(
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
        I::Item: AsRef<str>,
    {
        // dbg!("lookup_trait_definition_any_module");
        let (item_module_path, item_path, item_scope) = resolve_path(
            false,
            true,
            true,
            path.into_iter()
                .map(|seg| RustPathSegment {
                    ident: seg.as_ref().to_string(),
                    turbofish: Vec::new(),
                })
                .collect::<Vec<_>>(),
            self,
            current_module_path,
            current_module_path,
            current_scope_id,
        );
        let item_module = self
            .modules
            .iter()
            .find(|m| m.path == item_module_path)
            .unwrap();

        let trait_definiton = if let Some(item_scope) = &item_scope {
            let svd = item_module
                .scoped_various_definitions
                .iter()
                .find(|svd| &svd.0 == item_scope)
                .unwrap();
            svd.1
                .trait_definitons
                .iter()
                .find(|trait_def| trait_def.name == item_path[0].ident)
        } else {
            item_module
                .various_definitions
                .trait_definitons
                .iter()
                .find(|t| t.name == item_path[0].ident)
        };
        (
            item_module_path,
            item_scope,
            trait_definiton.unwrap().clone(),
        )
    }

    fn get_module_mut(&mut self, module_path: &[String]) -> &mut ModuleData {
        self.modules
            .iter_mut()
            .find(|m| m.path == module_path)
            .unwrap()
    }

    // This Doesn't/shouldn't look up methods as far as I can tell (methods are always handled directly in handle_expr_method_call) so rename
    // fn lookup_method_or_associated_fn(
    fn lookup_associated_fn(
        &self,
        item_generics: &[RustTypeParam],
        item_module_path: &[String],
        item_scope_id: &Option<Vec<usize>>,
        sub_path: &RustPathSegment,
        item_path_seg: &str,
        item_def: &ItemDefinition,
        // ) -> Option<PartialRustType> {
    ) -> Option<RustType> {
        // let impl_method = self.lookup_impl_item_item(
        //     item_generics,
        //     item_module_path,
        //     item_scope_id,
        //     sub_path,
        //     item_path_seg,
        //     item_def,
        // );

        let impl_method = self.lookup_impl_item_item2(item_def, sub_path);

        // let impl_method = if let Some((used, impl_method)) = impl_method {
        //     match impl_method.item {
        //         RustImplItemItem::Fn(private, static_, fn_info, js_fn) => {
        //             // If turbofish exists on fn path segment then use that for type params, otherwise use the unresolved params defined on the fn definition
        //             let fn_generics = if sub_path.turbofish.len() > 0 {
        //                 sub_path
        //                     .turbofish
        //                     .iter()
        //                     .enumerate()
        //                     .map(|(i, g)| RustTypeParam {
        //                         name: fn_info.generics[i].clone(),
        //                         type_: RustTypeParamValue::RustType(Box::new(g.clone())),
        //                     })
        //                     .collect::<Vec<_>>()
        //             } else {
        //                 // NOTE for now we are assuming turbofish must exist for generic items, until we implement a solution for getting type params that are resolved later in the code
        //                 assert!(fn_info.generics.len() == 0);
        //                 fn_info
        //                     .generics
        //                     .iter()
        //                     .map(|g| RustTypeParam {
        //                         name: g.clone(),
        //                         type_: RustTypeParamValue::Unresolved,
        //                     })
        //                     .collect::<Vec<_>>()
        //             };

        //             // Some(PartialRustType::RustType(RustType::Fn(
        //             //     Some(item_generics.clone()),
        //             //     fn_generics,
        //             //     item_module_path.clone(),
        //             //     RustTypeFnType::AssociatedFn(item_def.ident, sub_path.ident),
        //             // )))
        //             Some(RustType::Fn(
        //                 Some(item_generics.clone()),
        //                 fn_generics,
        //                 item_module_path.clone(),
        //                 item_scope_id.clone(),
        //                 RustTypeFnType::AssociatedFn(
        //                     item_def.ident.clone(),
        //                     sub_path.ident.clone(),
        //                 ),
        //             ))
        //         }
        //         RustImplItemItem::Const(_) => todo!(),
        //     }
        // } else {
        //     None
        // };
        // impl_method

        let impl_method = if let Some(impl_method) = impl_method {
            match impl_method.item {
                RustImplItemItemNoJs::Fn(static_, fn_info) => {
                    // If turbofish exists on fn path segment then use that for type params, otherwise use the unresolved params defined on the fn definition
                    let fn_generics = if !sub_path.turbofish.is_empty() {
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
                        assert!(fn_info.generics.is_empty());
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
                        Some(item_generics.to_vec()),
                        fn_generics,
                        item_module_path.to_vec(),
                        item_scope_id.clone(),
                        RustTypeFnType::AssociatedFn(
                            item_def.ident.clone(),
                            sub_path.ident.clone(),
                        ),
                    ))
                }
                RustImplItemItemNoJs::Const => todo!(),
            }
        } else {
            None
        };
        impl_method
    }

    fn lookup_impl_item_item2(
        &self,
        // item_generics: &Vec<RustTypeParam>,
        // item_module_path: &Vec<String>,
        // item_scope_id: &Option<Vec<usize>>,
        item_def: &ItemDefinition,
        sub_path: &RustPathSegment,
    ) -> Option<RustImplItemNoJs> {
        // let module = self
        //     .modules
        //     .iter()
        //     // .find(|m| &m.path == current_module)
        //     .find(|m| &m.path == item_module_path)
        //     .unwrap();

        let impl_method = item_def.impl_block_ids.iter().find_map(|impl_block_id| {
            // TODO also look for scoped impl blocks
            // TODO take into account item type params for generic impls

            // let module_impl_blocks = self.impl_blocks_simpl.iter();
            // let scoped_impl_blocks = module
            //     .scoped_various_definitions
            //     .iter()
            //     .map(|svd| &svd.2)
            //     .flatten();
            // module_impl_blocks
            //     .chain(scoped_impl_blocks)

            // TODO should we be looking through multiple blocks here or should be deuplicate `impl_blocks_simpl` after it is created??
            // I think we could dedupe (as long as we take into account scope) be it is impossible to have duplicate method names, however for impls like `impl<T> Foo for T` maybe we want to keep the impl blocks separate like they are in the original code??? Yes but we can still just dedupe the `RustImplBlockSimple` and keep the `JsImplBlock2` separate. It seems better to just look through multiple blocks here as that is easier than deduplicating and merging items.
            self.impl_blocks_simpl
                .iter()
                .filter(|ibs| &ibs.unique_id == impl_block_id)
                .find_map(|rust_impl_block_simple| {
                    rust_impl_block_simple
                        .rust_items
                        .iter()
                        .find(|rust_item| rust_item.ident == sub_path.ident)
                        .cloned()
                })
        });
        // dbg!(&impl_method);
        // dbg!(&item_def.ident);
        // dbg!(&sub_path.ident);
        // let impl_method = if let Some(impl_method) = impl_method {
        //     impl_method
        // } else {
        //     // dbg!(&global_data.scopes);
        //     dbg!(&item_generics);
        //     dbg!(&item_module_path);
        //     dbg!(&item_scope_id);
        //     dbg!(&sub_path);
        //     // dbg!(&item_name);
        //     dbg!(&item_def);
        //     panic!()
        // };
        impl_method
    }

    // TODO method should just take a RustType rather than specifically an item/struct
    // Looks up the `RustImplItem` eg method for a given item
    // TODO IMPORTANT not handling scoped impls for now for simplicity
    // fn lookup_impl_item_item(
    //     &self,
    //     item_generics: &Vec<RustTypeParam>,
    //     item_module_path: &Vec<String>,
    //     item_scope_id: &Option<Vec<usize>>,
    //     sub_path: &RustPathSegment,
    //     item_name: &String,
    //     item_def: &ItemDefinition,
    //     // ) -> Option<PartialRustType> {
    //     // ) -> Option<(bool, RustImplItem)> {
    // ) -> Option<RustImplItemNoJs> {
    //     // For now focus on supporting explicit gnerics ie turbofish etc so don't have to worry about unresolved types, and module level items so I don't have too much about complex scope shadowing behaviours.

    //     // Look for associated fn of item (struct or enum)

    //     // Look through all impl blocks which match item to find impl item which matches subpath name

    //     // First look for method in direct (non-trait) impls
    //     // TODO for generic structs we need to know the concrete types to know if we should match eg Foo<i32>
    //     // let scoped_impls = self.scopes.iter().map(|s| s.impl_blocks.iter()).flatten();

    //     let module = self
    //         .modules
    //         .iter()
    //         .find(|m| &m.path == item_module_path)
    //         .unwrap();
    //     // let scoped_impls = module.scoped_various_definitions.iter().find_map(|svd| {
    //     //     if let Some(scope_id) = &item_scope_id {
    //     //         if &svd.0 == scope_id {
    //     //             Some(svd.2)
    //     //         } else {
    //     //             None
    //     //         }
    //     //     } else {
    //     //         None
    //     //     }
    //     // });
    //     // let scoped_impls = if let Some(scoped_impls) = scoped_impls {
    //     //     scoped_impls
    //     // } else {
    //     //     Vec::new()
    //     // };
    //     // let scoped_non_trait_impl_method = self
    //     //     .impl_blocks
    //     //     .iter()
    //     //     // .chain(scoped_impls.iter())
    //     //     .find_map(|impl_block| {
    //     //         let types_match = struct_or_enum_types_match(
    //     //             &impl_block.target,
    //     //             &item_generics,
    //     //             &item_module_path,
    //     //             &item_scope_id,
    //     //             &item_def.ident,
    //     //         );

    //     //         // If we have a matching, non-trait impl block, look to see if it contains the method
    //     //         if types_match && impl_block.trait_.is_none() {
    //     //             impl_block
    //     //                 .items
    //     //                 .iter()
    //     //                 .find(|impl_item| impl_item.ident == sub_path.ident)
    //     //                 .cloned()
    //     //         } else {
    //     //             None
    //     //         }
    //     //     });

    //     // Now look for the method in trait impls
    //     // NOTE that while we are looking up a method for a particular `item_def`, we are matching on both concrete impls like `impl<T> Foo for Bar`, and generic impls like `impl<T> Foo for T` or `impl<T: Bar> Foo for T` in which case we need to check if our concrete `item_def` meets the trait bounds of `T`.
    //     // Say we have an impl block like `impl<T> Foo for T`, then we know our item/struct impls Foo and we can add the `impl<T> Foo for T` impl block to the list of impl blocks in which to look for the method.
    //     // However, for an impl block like `impl<T: Bar> Foo for T`, then we first need to determine whether our item/struct implements Bar.
    //     // So the first thing we do is find *all* the traits our item/struct implements
    //     // Note that this is a recursive process, ie once we have found that impl Foo via `impl<T: Bar> Foo for T`, we then assuming that `impl<T: Foo> Baz for T` appears before `impl<T: Bar> Foo for T` in the list of impl blocks, we would need to do another pass of the impl blocks to match it, now that we know we impl Foo. And we should repeatedly do another pass every time we match a new trait impl, until no new trait impls were matched.

    //     // Also note it isn't possible to calculate this out upfront eg in the first pass in `process_items`, because for generic structs it can depend on the concrete types of the generics so will need calculating individually on demand at the point we know the concrete type. For non-generic structs however I think we could calculate up front which will be worth doing at some point for better performance.

    //     // Only need to look in the same or child scopes of the item definition since an impl on an item cannot be in a parent scope
    //     // let possible_scopes = self.scopes.iter().rev().take_while(|s| {
    //     //     s.item_definitons
    //     //         .iter()
    //     //         .any(|item_def| &item_def.ident == item_name)
    //     // });
    //     let possible_scopes = if let Some(item_scope_id) = item_scope_id {
    //         let mut temp_scope_id = item_scope_id.clone();
    //         let mut scopes = Vec::new();
    //         while !temp_scope_id.is_empty() {
    //             let scope = module
    //                 .scoped_various_definitions
    //                 .iter()
    //                 .find(|svd| svd.0 == temp_scope_id)
    //                 .unwrap();
    //             scopes.push(scope.2.clone());
    //             temp_scope_id.pop();
    //         }
    //         scopes.into_iter().flatten().collect::<Vec<_>>()
    //     } else {
    //         Vec::new()
    //     };
    //     let mut all_impl_blocks = possible_scopes;
    //     all_impl_blocks.extend(self.impl_blocks_simpl.clone().into_iter());
    //     // let all_impl_blocks = self.impl_blocks.clone();
    //     dbg!(&all_impl_blocks);

    //     let found_traits = get_traits_implemented_for_item(
    //         &all_impl_blocks,
    //         item_module_path,
    //         item_scope_id,
    //         item_name,
    //     );

    //     // Now we know all the traits the item/struct impls, we can go through all the impl blocks and find the one (if any)
    //     let trait_impl_method = all_impl_blocks
    //         .iter()
    //         .find_map(|impl_block| {
    //             // TODO this needs extending to handle matching any target type, rather than just user structs
    //             match &impl_block.target {
    //                 RustType::TypeParam(rust_type_param) => {
    //                     // TODO should we be looking for/matching on found_traits in the `Foo` or `Bar` of `impl<T: Foo> Bar for T`??? I don't think it actually matter since we seem to be duplicating some work we already did in creating found_traits. NOTE but we must ensure we are matching the correct block, eg Bar might be in found_traits, but doesn't mean we should match because Foo might not be and we might actually get Bar from `impl<T: Baz> Bar for T`, so it is the trait bounds that we must match on.

    //                     // Get bounds on type param
    //                     let type_param_bounds = &impl_block
    //                         .generics
    //                         .iter()
    //                         .find(|generic| generic.ident == rust_type_param.name)
    //                         .unwrap()
    //                         .trait_bounds;

    //                     // If there are no trait bounds ie `impl<T> Foo for T` we always match
    //                     // Does our struct impl all of these traits?
    //                     let struct_impls_all_bounds = type_param_bounds
    //                         .iter()
    //                         .all(|type_param_bound| found_traits.contains(type_param_bound));

    //                     // If so then look for method in impl block
    //                     if struct_impls_all_bounds {
    //                         impl_block
    //                             // .items
    //                             .rust_items
    //                             .iter()
    //                             // .find(|(used, impl_item)| impl_item.ident == sub_path.ident)
    //                             .find(|impl_item| impl_item.ident == sub_path.ident)
    //                     } else {
    //                         None
    //                     }
    //                 }
    //                 RustType::StructOrEnum(
    //                     struct_type_params,
    //                     struct_module_path,
    //                     struct_scope_id,
    //                     struct_name,
    //                 ) => {
    //                     if let Some(impl_trait) = &impl_block.trait_ {
    //                         let types_match = struct_or_enum_types_match(
    //                             &impl_block.target,
    //                             &item_generics,
    //                             &item_module_path,
    //                             item_scope_id,
    //                             &item_def.ident,
    //                         );

    //                         // If types match then look for method in impl block
    //                         if types_match {
    //                             impl_block
    //                                 // .items
    //                                 .rust_items
    //                                 .iter()
    //                                 // .find(|(used, impl_item)| impl_item.ident == sub_path.ident)
    //                                 .find(|impl_item| impl_item.ident == sub_path.ident)
    //                         } else {
    //                             None
    //                         }
    //                         // TODO Trying to get the concrete params at this point doesn't make senese because quite often it is the argument(s) to the associated fn which will determine the concrete params
    //                     } else {
    //                         None
    //                     }
    //                 }
    //                 RustType::MutRef(_) => todo!(),
    //                 RustType::Ref(_) => todo!(),
    //                 _ => todo!(),
    //             }
    //         })
    //         .cloned();

    //     dbg!(&trait_impl_method);

    //     // Now we have all the impl blocks, we can look for the method in said impl blocks
    //     // We also need to check the traits themselves incase the method is a default implementation
    //     // let scoped_traits = self
    //     //     .scopes
    //     //     .iter()
    //     //     .rev()
    //     //     .map(|s| {
    //     //         s.trait_definitons
    //     //             .iter()
    //     //             .filter(|trait_def| found_traits.contains(&(None, trait_def.name.clone())))
    //     //     })
    //     //     .flatten();
    //     let module_level_traits = self
    //         .modules
    //         .iter()
    //         .map(|module| {
    //             module.trait_definitons.iter().filter(|trait_def| {
    //                 found_traits.contains(&(module.path.clone(), None, trait_def.name.clone()))
    //             })
    //         })
    //         .flatten();
    //     // TODO add default impl items to traits
    //     // let default_trait_method = scoped_traits
    //     //     .chain(module_level_traits)
    //     //     .find_map(|trait_def| {
    //     //         trait_def
    //     //             .items
    //     //             .iter()
    //     //             .find(|impl_item| impl_item.ident == sub_path.ident)
    //     //             .cloned()
    //     //     });

    //     // It is not possible to have impls with the same method name, so we should at most match 1 impl item/method
    //     // dbg!(&matched_trait_impl_blocks);
    //     // assert!(
    //     //     matched_trait_impl_blocks
    //     //         .iter()
    //     //         .filter_map(|impl_block| {
    //     //             impl_block
    //     //                 .items
    //     //                 .iter()
    //     //                 .find(|impl_item| impl_item.ident == sub_path.ident)
    //     //         })
    //     //         .count()
    //     //         <= 1
    //     // );
    //     // let module_level_impl_method = matched_trait_impl_blocks
    //     //     .iter()
    //     //     .find_map(|impl_block| {
    //     //         impl_block
    //     //             .items
    //     //             .iter()
    //     //             .find(|impl_item| impl_item.ident == sub_path.ident)
    //     //     })
    //     //     .cloned();

    //     // Use xor because we should not have both a scoped and module level impl method, only either or
    //     // let impl_method = scoped_non_trait_impl_method.xor(trait_impl_method);
    //     let impl_method = trait_impl_method;
    //     impl_method
    // }
}

#[allow(clippy::too_many_arguments)]
pub fn resolve_path(
    look_for_scoped_vars: bool,
    // TODO can we combine this with `look_for_scoped_vars`?
    look_for_scoped_items: bool,
    use_private_items: bool,
    mut segs: Vec<RustPathSegment>,
    // TODO replace GlobalData with `.modules` and `.scopes` to making setting up test cases easier
    global_data: &GlobalData,
    current_mod: &[String],
    // Only used to determine if current module is the original module
    orig_mod: &[String],
    current_scope_id: &Option<Vec<usize>>,
) -> (Vec<String>, Vec<RustPathSegment>, Option<Vec<usize>>) {
    debug!(segs = ?segs, "get_path_without_namespacing");

    // TODO I don't think we need to pass in the module `ModuleData` if we are already passing the `current_module` module path we can just use that to look it up each time, which might be less efficient since we shouldn't need to lookup the module if we haven't changed modules (though I think we are pretty much always changing modules except for use statements?), but we definitely don't want to pass in both. Maybe only pass in `module: &ModuleData` and not `current_module`
    // assert!(current_module == &module.path);

    let module = global_data
        .modules
        .iter()
        .find(|m| m.path == current_mod)
        .unwrap();

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

    let item_defined_in_module = module.item_defined_in_module(
        use_private_items || is_parent_or_same_module,
        &segs[0].ident,
    );

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
    let external_crate_names = ["web_prelude"];
    let path_is_external_crate = external_crate_names.iter().any(|cn| cn == &segs[0].ident);

    // TODO IMPORTANT we have two `is_scoped` vars here because we are using `get_path` in different contexts. `is_scoped_static` is for getting the path from static data, before syn -> JS parsing, and `is_scoped` is for use during the syn -> JS parsing. This needs thinking about, reconciling and simplifying. Should just stop using get_path for vars.
    let is_scoped_static_scope = if let Some(scope_id) = current_scope_id {
        // dbg!(scope_id);
        let mut temp_scope_id = scope_id.clone();
        // TODO initially wanted to not handle vars in this fn (`get_path`) but it doesn't seem possible without duplicating work given that an items and vars can shadow each other, we need to look through both the static and var scopes simultaneously to check if we have a var
        let mut is_var = false;
        let mut is_func = false;
        let mut is_item_def = false;
        let mut is_trait_def = false;
        while !temp_scope_id.is_empty() {
            let svd = module
                .scoped_various_definitions
                .iter()
                // TODO should this be `&svd.0 == temp_scope_id` ???
                .find(|svd| svd.0 == temp_scope_id);

            let svd = if let Some(svd) = svd {
                svd
            } else {
                println!("oh no, in get_path we couldn't find a scoped VariousDefinitions for one of the parent scopes of:");
                dbg!(scope_id);
                dbg!(&segs);
                dbg!(&module.scoped_various_definitions);
                panic!("");
            };
            // dbg!(&look_for_scoped_items);

            let static_scope = &svd.1;

            // NOTE the `segs.len() == 1` is important to differentiate between the receiver var `self` and the path to the current module `self::Foo` since the former will always be len == 1 and the latter len > 2
            is_var = look_for_scoped_vars && segs.len() == 1 && {
                // dbg!(&global_data.scopes);
                // dbg!(&scope_id);
                // dbg!(&segs[0].ident);
                // dbg!(&look_for_scoped_vars);

                let var_scope = global_data
                    .scopes
                    .iter()
                    .find(|s| s.scope_id == temp_scope_id)
                    .unwrap();
                var_scope
                    .variables
                    .iter()
                    .any(|var| var.name == segs[0].ident)
            };
            // dbg!(is_var);
            is_func = look_for_scoped_items
                && static_scope
                    .fn_info
                    .iter()
                    .any(|func| func.ident == segs[0].ident);

            // dbg!(&static_scope.item_definitons);
            // dbg!(&module.scoped_various_definitions);
            // dbg!(&segs[0].ident);
            // dbg!(&look_for_scoped_items);
            is_item_def = look_for_scoped_items
                && static_scope
                    .item_definitons
                    .iter()
                    .any(|item_def| item_def.ident == segs[0].ident);
            is_trait_def = look_for_scoped_items
                && static_scope
                    .trait_definitons
                    .iter()
                    .any(|trait_def| trait_def.name == segs[0].ident);

            if is_var || is_func || is_item_def || is_trait_def {
                break;
            }
            temp_scope_id.pop();
        }

        // A scoped item must be the first element in the segs, ie in the original module so we need `current_module == original_module`
        // TODO I don't think `&& current_mod == orig_mod` is necessary given look_for_scoped_vars and look_for_scoped_items
        if (is_var || is_func || is_item_def || is_trait_def) && current_mod == orig_mod {
            Some(temp_scope_id)
        } else {
            None
        }
    } else {
        None
    };

    // let is_scoped = global_data.scopes.iter().rev().any(|scope| {
    //     let is_func =
    //         look_for_scoped_items && scope.fns.iter().any(|func| func.ident == segs[0].ident);
    //     let is_item_def = look_for_scoped_items
    //         && scope
    //             .item_definitons
    //             .iter()
    //             .any(|item_def| item_def.ident == segs[0].ident);
    //     // TODO IMPORTANT
    //     // We cannot have a scoped item/fn definition with the same ident as a module, but we can have a scoped *var* with the same ident as a module/item/fn. I think Rust just chooses which one to use based on the context eg foo::bar must be an item/module, foo.bar() must be an instance/var, etc. To follow this approach would mean we need more context for this fn.
    //     // eg this is aloud:
    //     // use tracing;
    //     // let tracing = "ohno";
    //     // As far as I am aware the only common context in which a path might have length=1 is a use statement, which doesn't use this fn to resolve the path (though it probably should given we have to follow the same crate/self/super logic?) so for now just assume that if we match a scoped var name and the length is 1, then return the scoped var, even though technically it could be a module/item/fn eg this is valid: *NO* what about a simple `Foo {}` which is a path with length 1 where we could also have `let Foo = 5;`, which would make it impossible to decide which to return, eg module level struct (Some("crate"), ["Foo"]) vs scoped var (None, ["Foo"]).**
    //     // struct foo {}
    //     // fn main() {
    //     //     let foo = 5;
    //     // }
    //     // For scoped modules/items/fns there is currently no difference anyway since we currently just return Vec<RustPathSegment> regardless.
    //     // The main problem is the above example. However, the below is not valid which I believe demonstrates that it the ident must be unambigious if it can be used as eg a fn argument, and so it is indeed the context of where the path is being used eg `bar(foo);`, `let bar = foo;`, `foo {}`, etc which determines which thing to use. I think it will be non trivial to pass handle handle the different contexts to this fn.
    //     // struct foo;
    //     // fn main() {
    //     //     let foo = 5;
    //     // }
    //     // ** The distinction is between where the site of the path expects a definition (eg fn input type) and instances (eg assign to a variable). A slight complication is that both of the below are valid, just not simultaneously, but it still means that a path passed the the rhs of an assignment could be and instance *or* a definition, so we need to look for other if one doesn't exist, **but *only* if we have `struct foo;` and not `struct foo {}` because for the latter we *are* allowed both idents in scope, so need to ensure we choose the scoped var, in this case. This is as apposed to say the type of a fn input where we can always be sure to not look for scoped vars, only any items/fns.
    //     // let foo = 5;
    //     // let which = foo;
    //     // ...
    //     // struct foo;
    //     // let which = foo;
    //     // So I think maybe the trick is to pass an argument to this fn to say wether we should be considering vars and follow these rules:
    //     // 1. If including vars (eg simple 1-len path assignment) then look for a var first, else look for a struct, this way we will catch assigning `struct foo;` because a `foo` var can't exist in this case, and for `struct Foo {}` we will correctly pick the var first.
    //     // 2. If not inlcluding vars we simply don't have to look for vars.
    //     // Don't need both can just always do step 1?

    //     // self could be and instance or module path ie `fn foo(&self) { self }` or `self::MyStruct`. I can't think of any situations where a module path can
    //     let is_var = scope.variables.iter().any(|var| var.name == segs[0].ident)
    //         && segs.len() == 1
    //         && look_for_scoped_vars;

    //     // A scoped item must be the first element in the segs, ie in the original module so we need `current_module == original_module`
    //     // TODO I don't think `&& current_mod == orig_mod` is necessary given look_for_scoped_vars and look_for_scoped_items
    //     (is_func || is_item_def || is_var) && current_mod == orig_mod
    // });

    // dbg!(&global_data.scopes);

    // TODO not sure why we need use_private_items here
    // if use_private_items && is_scoped {
    // if is_scoped || is_scoped_static {
    // dbg!(&is_scoped_static_scope);
    if is_scoped_static_scope.is_some() {
        // Variables and scoped items
        // Need to handle scoped vars and items first, otherwise when handling as module paths, we would always first have to check if the path is a scoped var/item

        // If we are returning a scoped var/item, no recursion should have occured so we should be in the same module
        assert!(current_mod == orig_mod);
        // (current_mod.clone(), segs, is_scoped_static)
        (current_mod.to_vec(), segs, is_scoped_static_scope)
    } else if item_defined_in_module {
        (current_mod.to_vec(), segs, None)
    } else if segs[0].ident == "super" {
        // TODO if a module level item name is shadowed by an item in a fn scope, then module level item needs to be namespaced
        segs.remove(0);

        let mut current_module = current_mod.to_vec();
        current_module.pop();

        resolve_path(
            false,
            false,
            true,
            segs,
            global_data,
            &current_module,
            orig_mod,
            &None,
        )
    } else if segs[0].ident == "self" {
        // NOTE private items are still accessible from the module via self
        segs.remove(0);

        resolve_path(
            false,
            false,
            true,
            segs,
            global_data,
            current_mod,
            orig_mod,
            &None,
        )
    } else if segs[0].ident == "crate" {
        let current_module = vec!["crate".to_string()];

        segs.remove(0);

        resolve_path(
            false,
            false,
            true,
            segs,
            global_data,
            &current_module,
            orig_mod,
            &None,
        )
    } else if path_starts_with_sub_module {
        // Path starts with a submodule of the current module
        let mut submod_path = current_mod.to_vec();
        submod_path.push(segs[0].ident.to_string());

        segs.remove(0);

        resolve_path(
            false,
            false,
            false,
            segs,
            global_data,
            &submod_path,
            orig_mod,
            &None,
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
            false,
            true,
            use_segs,
            global_data,
            // &new_mod,
            current_mod,
            // &use_mapping.1.clone(),
            orig_mod,
            &None,
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
            false,
            true,
            segs,
            global_data,
            &current_module,
            orig_mod,
            &None,
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
            (vec!["prelude_special_case".to_string()], segs, None)
        } else {
            dbg!("resolve_path couldn't find path");
            // dbg!(module);
            dbg!(current_mod);
            dbg!(current_scope_id);
            dbg!(segs);
            panic!()
        }
    }
}
