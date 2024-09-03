use std::rc::Rc;

use syn::{
    FnArg, GenericArgument, GenericParam, ImplItem, Pat, PathArguments, ReturnType, TraitItem,
    Type, TypeParamBound, Visibility,
};
use syn::{ItemEnum, ItemImpl, ItemStruct, ItemTrait, Member};
use tracing::debug;

use crate::make_item_definitions::{resolve_path, RustMod};
use crate::{
    duplicate_namespacing::Duplicate,
    make_item_definitions::{self},
    RustPathSegment, RUST_PRELUDE_MODULE_PATH,
};
use crate::{
    handle_syn::{GlobalData, RustType2, RustTypeImplTrait2, RustTypeParam2, RustTypeParamValue2},
    js_ast::Ident,
    make_item_definitions::{
        look_for_module_in_crates, ExprRef, FnInfoSyn, ImplItemExprStmtRefs, ItemDefNoTypes,
        StructOrEnumDefitionInfo,
    },
};
use crate::{ItemRef, StmtsRef};

// This simply coverts the list/Vec of item defs to a list/Vec of item defs with the type fields populated (references to other items in the list/Vec) while presevering the order (because we actually get the index from the input Vec, even though we will use it to point to items in the output Vec).
// However, because we need to know which scoped items are in scope at any given point, we can't just iterate directly over the Vec<ItemActual>, we need to instead iterate over the ItemRef tree, looking for Items.
// IMPORTANT However, means we need to be careful to preserve the order of the original Vec<ItemActual>. The best approach it probably to create an "empty" Vec initially, and then directly insert the updated defs at the position according to their index.
pub fn update_item_defs(
    crates: &[RustMod],
    item_defs_no_types: Vec<ItemDefNoTypes>,
    current_module: &[String],
    in_scope: bool,
    duplicates: Vec<Duplicate>,
) -> Vec<ItemDef> {
    // (Vec<ModuleData>, Vec<RustImplBlockSimple>)

    let mut scoped_items = Vec::new();

    // NOTE type params cannot be used inside a child fn
    // Used for tracking whether a type param is used like `T::associated_fn()`
    // (name, used)
    // NOTE type params cannot be used inside a child fn
    // Used for tracking whether a type param is used like `T::associated_fn()`. Push a new Vec<(String, bool)> containing the type param names and false for each function eg `fn foo<T, U>() {}` -> `[(T, false), (U, false)]` and then set the bool to true if we find the type param being using like `T::associated_fn()`, and will therefore need to pass the type params as args to the compiled JS fn.
    // (name, used)
    // TODO be clear about why we need to add an initial "scope" (ie vec![vec![]] not vec![])
    let mut type_params: Vec<Vec<(String, bool, Vec<usize>)>> = vec![vec![]];

    let mut updated_item_defs = Vec::with_capacity(item_defs_no_types.len());
    updated_item_defs.resize_with(item_defs_no_types.len(), || ItemDef::None);

    for rust_mod in crates {
        for item_ref in &rust_mod.items {
            update_item_defs_recurisve_individual_item(
                item_ref,
                crates,
                &item_defs_no_types,
                &rust_mod.module_path,
                &mut updated_item_defs,
                &mut scoped_items,
                &mut type_params,
                in_scope,
                &duplicates,
            );
        }
    }

    updated_item_defs
}

fn update_item_defs_recurisve_individual_item(
    item_ref: &ItemRef,
    crates: &[RustMod],
    item_defs_no_types: &[ItemDefNoTypes],
    current_module: &[String],
    updated_item_defs: &mut [ItemDef],
    // TODO use &ItemRef
    scoped_items: &mut Vec<Vec<ItemRef>>,
    // NOTE type params cannot be used inside a child fn
    // Used for tracking whether a type param is used like `T::associated_fn()`
    // (name, used)
    type_params: &mut Vec<Vec<(String, bool, Vec<usize>)>>,
    in_scope: bool,
    duplicates: &[Duplicate],
) {
    match item_ref {
        ItemRef::StructOrEnum(index) => {
            // TODO moving the def means we no longer have a complete list of items to lookup eg Type::Path names in. Solution is to have optional fields on the def and update it in place? Or could in theory first look in the old list and if we only find `None` then look in the new list instead? Might get a bit messy with recursive types since the old definition will have been removed but the new one not pushed yet.
            // Could store a pre/post ItemDef enum and then just switch from pre to post as we iterate through the Vec. The problem is that we will want a mutable ref to the element at the same time as an immutable ref to the whole Vec. Same problem with storing different fields as `Option`al.
            // Need three variants: Pre, Post, None, so that we can take out the pre, mutate it, the put it back. This could/would cause problems for recursive types. I think is all we really need is the item ident and pub, which are Copy so could just store them on None also??
            // We could just have update_item_def() return the "new bits" and *then* update the item def element after we are no longer using the immutable ref to the whole Vec. Would need another type/wrapper for this though since fn/struct/etc will all return different "bits"... no that still doesn't work because we would need to take out the element first in order to create the "bits" from it... NO I think we only need to take out/copy the ident when creating the bits, then we can move the rest after `update_item_def()`. Nope we need stuff like the syn fields.
            // I think the solution is actually to move all the bits we need out first, only leaving the ident and is_pub (ie a None variant that only contains the ident).
            // The problem is in resolve_path we need to get the RustMod for the current module (using look_for_module_in_items) so that we can check if the current seg ident appears in the module, for the flag `item_defined_in_module`. But this means we need to search through eg fn stmts `StmtRef`s looking for modules and the fn stmts are stored on the fn def.
            // Maybe we should be storing fns's `StmtRef`s in `ItemRef::Fn`??
            // let item = mem::replace(&mut item_defs_no_types[*index], ItemActual::None);
            let item = item_defs_no_types[*index].clone();
            updated_item_defs[*index] = update_item_def(
                item,
                current_module,
                crates,
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
            // TODO If the function is generic, we need to look through the fn body and determine if any type param gets used directly like `T::associated_fn()` because in that case we need to include the type params as the first inputs, and then also for fn calls pass the concrete types as the first args. Given the fn calls can appear before the fn, we can't just wait until handle_fn_item to determine this.

            // let item = mem::replace(&mut item_defs_no_types[*index], ItemActual::None);
            let item = item_defs_no_types[*index].clone();
            updated_item_defs[*index] = update_item_def(
                item,
                current_module,
                crates,
                item_defs_no_types,
                scoped_items,
                in_scope,
                duplicates,
            );

            if in_scope {
                scoped_items.last_mut().unwrap().push(item_ref.clone());
            }

            let fn_def = match &updated_item_defs[*index] {
                ItemDef::Fn(fn_def) => fn_def,
                _ => todo!(),
            };
            let fn_stmts = fn_def.stmts.clone();

            // Item in fn body are scoped so create a new scope
            scoped_items.push(Vec::new());
            // TODO unlike items, type params can't be used in all children eg child fns. Do we need some mechanism for checking this or will cargo check simply prevent any problems?
            // TODO ideally avoiding cloning and just store a mutable ref we can update?
            type_params.push(fn_def.sig.generics.clone());

            update_item_defs_recurisve_stmts(
                &fn_stmts,
                crates,
                item_defs_no_types,
                current_module,
                updated_item_defs,
                scoped_items,
                type_params,
                true,
                duplicates,
            );

            // If any of the fn's type params were used directly, record this on the fn_def
            let mut fn_def = match &mut updated_item_defs[*index] {
                ItemDef::Fn(fn_def) => fn_def.clone(),
                _ => todo!(),
            };

            // TODO Avoid this mess. Can't mutate Rc (fn_def.sig is Rc) so try to pass the used type params into update_item_def so they can be used when creating the Rc.
            let mut sig = (*fn_def.sig).clone();
            sig.generics = type_params.pop().unwrap();
            fn_def.sig = Rc::new(sig);
            updated_item_defs[*index] = ItemDef::Fn(fn_def);
            scoped_items.pop();
        }
        ItemRef::Const(index) => {
            // let item = mem::replace(&mut item_defs_no_types[*index], ItemActual::None);
            let item = item_defs_no_types[*index].clone();
            updated_item_defs[*index] = update_item_def(
                item,
                current_module,
                crates,
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
                crates,
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
                    crates,
                    item_defs_no_types,
                    &rust_mod.module_path,
                    updated_item_defs,
                    scoped_items,
                    type_params,
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
                crates,
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
                ItemDef::Impl(rust_impl_block) => {
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
                    crates,
                    item_defs_no_types,
                    current_module,
                    updated_item_defs,
                    scoped_items,
                    type_params,
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
    crates: &[RustMod],
    item_defs_no_types: &[ItemDefNoTypes],
    current_module: &[String],
    updated_item_defs: &mut [ItemDef],
    // TODO use &ItemRef
    scoped_items: &mut Vec<Vec<ItemRef>>,
    // NOTE type params cannot be used inside a child fn
    // Used for tracking whether a type param is used like `T::associated_fn()`
    // (name, used, trait bound indexes (not used, just easier to avoid removing then re-including it))
    type_params: &mut Vec<Vec<(String, bool, Vec<usize>)>>,
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
                    crates,
                    item_defs_no_types,
                    current_module,
                    updated_item_defs,
                    scoped_items,
                    type_params,
                    in_scope,
                    duplicates,
                );
            }
            StmtsRef::Expr(expr_ref, _) => {
                update_item_defs_recurisve_individual_expr(
                    expr_ref,
                    crates,
                    item_defs_no_types,
                    current_module,
                    updated_item_defs,
                    scoped_items,
                    type_params,
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
    crates: &[RustMod],
    item_defs_no_types: &[ItemDefNoTypes],
    current_module: &[String],
    updated_item_defs: &mut [ItemDef],
    // TODO use &ItemRef
    scoped_items: &mut Vec<Vec<ItemRef>>,
    // NOTE type params cannot be used inside a child fn
    // Used for tracking whether a type param is used like `T::associated_fn()`
    // (name, used)
    type_params: &mut Vec<Vec<(String, bool, Vec<usize>)>>,
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
                crates,
                item_defs_no_types,
                current_module,
                updated_item_defs,
                scoped_items,
                type_params,
                in_scope,
                duplicates,
            );
        }
        ExprRef::Break(_) => {}
        ExprRef::Call(rust_expr_call) => {
            update_item_defs_recurisve_individual_expr(
                &rust_expr_call.func,
                crates,
                item_defs_no_types,
                current_module,
                updated_item_defs,
                scoped_items,
                type_params,
                in_scope,
                duplicates,
            );
        }
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
        ExprRef::Path(rust_expr_path) => {
            // TODO check path isn't a user type, var, etc shadowing the type param name
            for (name, used, _) in type_params.last_mut().unwrap() {
                if rust_expr_path.path.segments[0].ident == name {
                    *used = true;
                }
            }
        }
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
pub enum ItemDefRc {
    StructEnum(Rc<StructEnumDef>),
    Fn(Rc<FnDef>),
    Const(Rc<ConstDef>),
    Trait(Rc<TraitDef>),
    Impl(Rc<ImplBlockDef>),
    None,
}
impl ItemDefRc {
    pub fn ident(&self) -> &str {
        match self {
            ItemDefRc::StructEnum(def) => &def.ident,
            ItemDefRc::Fn(def) => &def.sig.ident,
            ItemDefRc::Const(def) => &def.ident,
            ItemDefRc::Trait(def) => &def.ident,
            ItemDefRc::Impl(_) => panic!(),
            ItemDefRc::None => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemDef {
    StructEnum(StructEnumDef),
    Fn(FnDef),
    Const(ConstDef),
    Trait(TraitDef),
    Impl(ImplBlockDef),
    None,
}
impl ItemDef {
    pub fn ident(&self) -> &str {
        match self {
            ItemDef::StructEnum(def) => &def.ident,
            ItemDef::Fn(def) => &def.sig.ident,
            ItemDef::Const(def) => &def.ident,
            ItemDef::Trait(def) => &def.ident,
            ItemDef::Impl(_) => panic!(),
            ItemDef::None => panic!(),
        }
    }
}

fn update_item_def(
    item: ItemDefNoTypes,
    module_path: &[String],
    item_refs: &[RustMod],
    item_actual_defs_copy: &[ItemDefNoTypes],
    scoped_items: &[Vec<ItemRef>],
    in_scope: bool,
    duplicates: &[Duplicate],
) -> ItemDef {
    match item {
        ItemDefNoTypes::StructEnum(item_def) => {
            // TODO we have the same `let generics = ...` repeated a few times and they should be filtering out lifetimes like below
            let generics = item_def
                .syn_generics
                .params
                .into_iter()
                .filter_map(|param| {
                    match param {
                        GenericParam::Lifetime(_) => None,
                        GenericParam::Type(type_param) => {
                            let bound_indexes = type_param
                                .bounds
                                .into_iter()
                                .map(|bound| {
                                    match bound {
                                        TypeParamBound::Trait(trait_bound) => {
                                            let (
                                                _trait_module_path,
                                                _trait_item_path,
                                                _trait_item_scope,
                                                trait_index,
                                            ) = make_item_definitions::resolve_path(
                                                true,
                                                true,
                                                trait_bound
                                                    .path
                                                    .segments
                                                    .into_iter()
                                                    .map(|seg| {
                                                        RustPathSegment {
                                                            ident: seg.ident.to_string(),
                                                            // TODO
                                                            turbofish: vec![],
                                                        }
                                                    })
                                                    .collect(),
                                                item_refs,
                                                item_actual_defs_copy,
                                                module_path,
                                                module_path,
                                                scoped_items,
                                            );
                                            trait_index.unwrap()
                                        }
                                        TypeParamBound::Lifetime(_) => todo!(),
                                        TypeParamBound::Verbatim(_) => todo!(),
                                        _ => todo!(),
                                    }
                                })
                                .collect::<Vec<_>>();
                            Some((type_param.ident.to_string(), bound_indexes))
                        }
                        GenericParam::Const(_) => todo!(),
                    }
                })
                .collect::<Vec<_>>();

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
                                            &[],
                                            &generics,
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
                                        &[],
                                        &generics,
                                        module_path,
                                        item_refs,
                                        item_actual_defs_copy,
                                        scoped_items,
                                    )
                                })
                                .collect::<Vec<_>>(),
                        )
                    };
                    StructEnumUniqueInfo2::Struct(StructDefUniqueInfo {
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
                                        &[],
                                        & generics,
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
                    StructEnumUniqueInfo2::Enum(EnumDefUniqueInfo {
                        members: members_for_scope,
                        syn_object: enum_def_info,
                    })
                }
            };

            let js_name = if in_scope {
                Ident::Syn(item_def.ident.clone())
            } else if let Some(dup) = duplicates
                .iter()
                .find(|dup| item_def.ident == dup.name && dup.original_module_path == module_path)
            {
                Ident::Deduped(dup.namespace.clone())
            } else {
                Ident::Syn(item_def.ident.clone())
            };

            ItemDef::StructEnum(StructEnumDef {
                ident: item_def.ident.to_string(),
                js_name,
                is_copy: item_def.is_copy,
                is_pub: item_def.is_pub,
                generics,
                struct_or_enum_info: _new_struct_or_enum_info,
                impl_block_ids: Vec::new(),
                traits: Vec::new(),
            })
        }
        ItemDefNoTypes::Fn(fn_info) => {
            // let item_fn = match &fn_info.syn {
            //     make_item_definitions::FnInfoSyn::Standalone(item_fn) => item_fn,
            //     make_item_definitions::FnInfoSyn::Impl(_) => todo!(),
            // };

            let generics = fn_info
                .syn_generics
                .params
                .into_iter()
                .map(|param| {
                    match param {
                        GenericParam::Lifetime(_) => todo!(),
                        GenericParam::Type(type_param) => {
                            let bound_indexes = type_param
                                .bounds
                                .into_iter()
                                .map(|bound| {
                                    match bound {
                                        TypeParamBound::Trait(trait_bound) => {
                                            let (
                                                _trait_module_path,
                                                _trait_item_path,
                                                _trait_item_scope,
                                                trait_index,
                                            ) = make_item_definitions::resolve_path(
                                                true,
                                                true,
                                                trait_bound
                                                    .path
                                                    .segments
                                                    .into_iter()
                                                    .map(|seg| {
                                                        RustPathSegment {
                                                            ident: seg.ident.to_string(),
                                                            // TODO
                                                            turbofish: vec![],
                                                        }
                                                    })
                                                    .collect(),
                                                item_refs,
                                                item_actual_defs_copy,
                                                module_path,
                                                module_path,
                                                scoped_items,
                                            );
                                            trait_index.unwrap()
                                        }
                                        TypeParamBound::Lifetime(_) => todo!(),
                                        TypeParamBound::Verbatim(_) => todo!(),
                                        _ => todo!(),
                                    }
                                })
                                .collect::<Vec<_>>();
                            (type_param.ident.to_string(), false, bound_indexes)
                        }
                        GenericParam::Const(_) => todo!(),
                    }
                })
                .collect::<Vec<_>>();

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
                            &[],
                            &generics
                                .iter()
                                .cloned()
                                .map(|(name, used, trait_bounds)| (name, trait_bounds))
                                .collect::<Vec<_>>(),
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
                    &[],
                    &generics
                        .iter()
                        .cloned()
                        .map(|(name, used, trait_bounds)| (name, trait_bounds))
                        .collect::<Vec<_>>(),
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

            ItemDef::Fn(FnDef {
                is_pub: fn_info.is_pub,
                stmts: fn_info.stmts,
                syn: fn_info.syn,
                sig: Rc::new(FnSigDef {
                    ident: fn_info.ident.to_string(),
                    js_name,

                    inputs_types,
                    generics,
                    return_type,
                }),
            })
        }
        ItemDefNoTypes::Const(const_def) => {
            let rust_type = parse_types_for_populate_item_definitions(
                &const_def.syn_object.ty,
                &[],
                &[],
                module_path,
                item_refs,
                item_actual_defs_copy,
                scoped_items,
            );

            let js_name = if in_scope {
                Ident::Syn(const_def.ident.clone())
            } else if let Some(dup) = duplicates
                .iter()
                .find(|dup| const_def.ident == dup.name && dup.original_module_path == module_path)
            {
                Ident::Deduped(dup.namespace.clone())
            } else {
                Ident::Syn(const_def.ident.clone())
            };

            ItemDef::Const(ConstDef {
                ident: const_def.ident.to_string(),
                js_name,
                is_pub: const_def.is_pub,
                type_: rust_type,
                expr: const_def.expr,
            })
        }
        ItemDefNoTypes::Trait(trait_def) => {
            // Currently trait defs don't store any info other than the name, so we don't need to do anything
            // dbg!(&trait_def);

            let js_name = if in_scope {
                Ident::Syn(trait_def.ident.clone())
            } else {
                let in_module_level_duplicates = duplicates.iter().find(|dup| {
                    trait_def.ident == dup.name && dup.original_module_path == module_path
                });

                if let Some(dup) = in_module_level_duplicates {
                    Ident::Deduped(dup.namespace.clone())
                } else {
                    Ident::Syn(trait_def.ident.clone())
                }
            };

            // TODO lot's of duplication with default_impls below
            let items = trait_def
                .syn
                .clone()
                .items
                .into_iter()
                .map(|trait_item| {
                    let trait_item_fn = match trait_item {
                        TraitItem::Const(_) => todo!(),
                        TraitItem::Fn(trait_item_fn) => trait_item_fn,
                        TraitItem::Type(_) => todo!(),
                        TraitItem::Macro(_) => todo!(),
                        TraitItem::Verbatim(_) => todo!(),
                        _ => todo!(),
                    };
                    let generics = trait_item_fn
                        .sig
                        .generics
                        .params
                        .into_iter()
                        .map(|param| {
                            match param {
                                GenericParam::Lifetime(_) => todo!(),
                                GenericParam::Type(type_param) => {
                                    let bound_indexes = type_param
                                        .bounds
                                        .into_iter()
                                        .map(|bound| {
                                            match bound {
                                                TypeParamBound::Trait(trait_bound) => {
                                                    let (
                                                        _trait_module_path,
                                                        _trait_item_path,
                                                        _trait_item_scope,
                                                        trait_index,
                                                    ) = make_item_definitions::resolve_path(
                                                        true,
                                                        true,
                                                        trait_bound
                                                            .path
                                                            .segments
                                                            .into_iter()
                                                            .map(|seg| {
                                                                RustPathSegment {
                                                                    ident: seg.ident.to_string(),
                                                                    // TODO
                                                                    turbofish: vec![],
                                                                }
                                                            })
                                                            .collect(),
                                                        item_refs,
                                                        item_actual_defs_copy,
                                                        module_path,
                                                        module_path,
                                                        scoped_items,
                                                    );
                                                    trait_index.unwrap()
                                                }
                                                TypeParamBound::Lifetime(_) => todo!(),
                                                TypeParamBound::Verbatim(_) => todo!(),
                                                _ => todo!(),
                                            }
                                        })
                                        .collect::<Vec<_>>();
                                    (type_param.ident.to_string(), bound_indexes)
                                }
                                GenericParam::Const(_) => todo!(),
                            }
                        })
                        .collect::<Vec<_>>();

                    let inputs_types = trait_item_fn
                        .sig
                        .inputs
                        .iter()
                        .map(|input| match input {
                            FnArg::Receiver(receiver) => {
                                // For `self`, `&self`, `&mut self` receiver.ty will be `Self`, `&Self`, `&mut Self`
                                // For a default impl we can't know what self/Self will be and while for the fn body we only need to know about the trait methods, if &Self etc is returned then we do need to know the actual concrete type. Given this we need to use RustType::Self.
                                // TODO This means we need to ensure we don't attempt to resolve RustType::Self during the default impl fn and instead treat it as `impl Trait`, but once it has been returned from the trait fn call, we can coerce it to a concrete type. This will be complicated in cases like returning from a default trait fn within another trait fn. I think the solution is to look at the receiver - if it is a concrete type then when can coerce, otherwise not.
                                // Surely this is the same for `impl Foo for T {}` trait impls? Kind of, it will be an unresolved RustType::TypeParam.
                                (
                                    true,
                                    receiver.mutability.is_some(),
                                    "self".to_string(),
                                    RustType::Self_,
                                )
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
                                    &[],
                                    &generics,
                                    module_path,
                                    item_refs,
                                    item_actual_defs_copy,
                                    scoped_items,
                                ),
                            ),
                        })
                        .collect::<Vec<_>>();

                    let return_type = match &trait_item_fn.sig.output {
                        ReturnType::Default => RustType::Unit,
                        ReturnType::Type(_, type_) => parse_types_for_populate_item_definitions(
                            type_,
                            &[],
                            &generics,
                            module_path,
                            item_refs,
                            item_actual_defs_copy,
                            scoped_items,
                        ),
                    };

                    let js_name = if in_scope {
                        Ident::Syn(trait_item_fn.sig.ident.clone())
                    } else {
                        let in_module_level_duplicates = duplicates.iter().find(|dup| {
                            trait_item_fn.sig.ident == dup.name
                                && dup.original_module_path == module_path
                        });

                        if let Some(dup) = in_module_level_duplicates {
                            Ident::Deduped(dup.namespace.clone())
                        } else {
                            Ident::Syn(trait_item_fn.sig.ident.clone())
                        }
                    };

                    let static_ =
                        !matches!(trait_item_fn.sig.inputs.first(), Some(FnArg::Receiver(_)));

                    TraitItemDef::Fn(Rc::new(FnSigDef {
                        js_name,
                        ident: trait_item_fn.sig.ident.to_string(),
                        inputs_types,
                        generics: generics
                            .iter()
                            .cloned()
                            .map(|(name, trait_bounds)| (name, false, trait_bounds))
                            .collect::<Vec<_>>(),
                        return_type,
                    }))
                })
                .collect();

            let default_impls = trait_def
                .default_impls
                .into_iter()
                .map(|fn_info| {
                    let generics = fn_info
                        .syn_generics
                        .params
                        .into_iter()
                        .map(|param| {
                            match param {
                                GenericParam::Lifetime(_) => todo!(),
                                GenericParam::Type(type_param) => {
                                    let bound_indexes = type_param
                                        .bounds
                                        .into_iter()
                                        .map(|bound| {
                                            match bound {
                                                TypeParamBound::Trait(trait_bound) => {
                                                    let (
                                                        _trait_module_path,
                                                        _trait_item_path,
                                                        _trait_item_scope,
                                                        trait_index,
                                                    ) = make_item_definitions::resolve_path(
                                                        true,
                                                        true,
                                                        trait_bound
                                                            .path
                                                            .segments
                                                            .into_iter()
                                                            .map(|seg| {
                                                                RustPathSegment {
                                                                    ident: seg.ident.to_string(),
                                                                    // TODO
                                                                    turbofish: vec![],
                                                                }
                                                            })
                                                            .collect(),
                                                        item_refs,
                                                        item_actual_defs_copy,
                                                        module_path,
                                                        module_path,
                                                        scoped_items,
                                                    );
                                                    trait_index.unwrap()
                                                }
                                                TypeParamBound::Lifetime(_) => todo!(),
                                                TypeParamBound::Verbatim(_) => todo!(),
                                                _ => todo!(),
                                            }
                                        })
                                        .collect::<Vec<_>>();
                                    (type_param.ident.to_string(), bound_indexes)
                                }
                                GenericParam::Const(_) => todo!(),
                            }
                        })
                        .collect::<Vec<_>>();

                    let inputs_types = fn_info
                        .signature
                        .inputs
                        .iter()
                        .map(|input| match input {
                            FnArg::Receiver(receiver) => {
                                // For `self`, `&self`, `&mut self` receiver.ty will be `Self`, `&Self`, `&mut Self`
                                // For a default impl we can't know what self/Self will be and while for the fn body we only need to know about the trait methods, if &Self etc is returned then we do need to know the actual concrete type. Given this we need to use RustType::Self.
                                // TODO This means we need to ensure we don't attempt to resolve RustType::Self during the default impl fn and instead treat it as `impl Trait`, but once it has been returned from the trait fn call, we can coerce it to a concrete type. This will be complicated in cases like returning from a default trait fn within another trait fn. I think the solution is to look at the receiver - if it is a concrete type then when can coerce, otherwise not.
                                // Surely this is the same for `impl Foo for T {}` trait impls? Kind of, it will be an unresolved RustType::TypeParam.
                                (
                                    true,
                                    receiver.mutability.is_some(),
                                    "self".to_string(),
                                    RustType::Self_,
                                )
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
                                    &[],
                                    &generics,
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
                            &[],
                            &generics,
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
                            fn_info.signature.ident == dup.name
                                && dup.original_module_path == module_path
                        });

                        if let Some(dup) = in_module_level_duplicates {
                            Ident::Deduped(dup.namespace.clone())
                        } else {
                            Ident::Syn(fn_info.signature.ident.clone())
                        }
                    };

                    let trait_item_fn = match &fn_info.syn {
                        FnInfoSyn::Standalone(_) => todo!(),
                        FnInfoSyn::Impl(_) => todo!(),
                        FnInfoSyn::Trait(trait_item_fn) => trait_item_fn,
                    };

                    let static_ =
                        !matches!(trait_item_fn.sig.inputs.first(), Some(FnArg::Receiver(_)));

                    RustImplItemNoJs {
                        ident: fn_info.ident.to_string(),
                        item: RustImplItemItemNoJs::Fn(
                            static_,
                            FnDef {
                                is_pub: fn_info.is_pub,
                                stmts: fn_info.stmts,
                                syn: fn_info.syn,
                                sig: Rc::new(FnSigDef {
                                    ident: fn_info.ident.to_string(),
                                    js_name,
                                    inputs_types,
                                    generics: generics
                                        .iter()
                                        .cloned()
                                        .map(|(name, trait_bounds)| (name, false, trait_bounds))
                                        .collect::<Vec<_>>(),
                                    return_type,
                                }),
                            },
                        ),
                    }
                })
                .collect();

            ItemDef::Trait(TraitDef {
                ident: trait_def.ident.to_string(),
                js_name,
                is_pub: trait_def.is_pub,
                items,
                syn: trait_def.syn,
                default_impls,
            })
        }
        ItemDefNoTypes::Impl(item_impl, impl_items_refs) => {
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
                            name: target_type_param.ident,
                            trait_bounds: target_type_param.trait_bounds,
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
                    if target_item_module == [RUST_PRELUDE_MODULE_PATH] {
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
                            let generics = impl_item_fn
                                .clone()
                                .sig
                                .generics
                                .params
                                .into_iter()
                                .map(|param| {
                                    match param {
                                        GenericParam::Lifetime(_) => todo!(),
                                        GenericParam::Type(type_param) => {
                                            let bound_indexes =
                                                type_param
                                                    .bounds
                                                    .into_iter()
                                                    .map(|bound| {
                                                        match bound {
                                                            TypeParamBound::Trait(trait_bound) => {
                                                                let (
                                                _trait_module_path,
                                                _trait_item_path,
                                                _trait_item_scope,
                                                trait_index,
                                            ) = make_item_definitions::resolve_path(
                                                true,
                                                true,
                                                trait_bound
                                                    .path
                                                    .segments
                                                    .into_iter()
                                                    .map(|seg| {
                                                        RustPathSegment {
                                                            ident: seg.ident.to_string(),
                                                            // TODO
                                                            turbofish: vec![],
                                                        }
                                                    })
                                                    .collect(),
                                                item_refs,
                                                item_actual_defs_copy,
                                                module_path,
                                                module_path,
                                                scoped_items,
                                            );
                                                                trait_index.unwrap()
                                                            }
                                                            TypeParamBound::Lifetime(_) => todo!(),
                                                            TypeParamBound::Verbatim(_) => todo!(),
                                                            _ => todo!(),
                                                        }
                                                    })
                                                    .collect::<Vec<_>>();
                                            (type_param.ident.to_string(), bound_indexes)
                                        }
                                        GenericParam::Const(_) => todo!(),
                                    }
                                })
                                .collect::<Vec<_>>();

                            let impl_block_generics =
                                rust_impl_block_generics.iter().map(|g| g.ident.clone());

                            let impl_block_generics2 = rust_impl_block_generics
                                .iter()
                                .map(|g| (g.ident.clone(), g.trait_bounds.clone()))
                                .collect::<Vec<_>>();

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
                                            &impl_block_generics2,
                                            &generics,
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
                                        &impl_block_generics2,
                                        &generics,
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
                                FnDef {
                                    is_pub,
                                    syn: FnInfoSyn::Impl(impl_item_fn.clone()),
                                    stmts: match exprs_stmts_refs {
                                        ImplItemExprStmtRefs::Fn(stmt_refs) => stmt_refs,
                                        ImplItemExprStmtRefs::Const => todo!(),
                                    },
                                    sig: Rc::new(FnSigDef {
                                        js_name,
                                        ident: item_name.clone(),
                                        inputs_types,
                                        generics: generics
                                            .iter()
                                            .cloned()
                                            .map(|(name, trait_bounds)| (name, false, trait_bounds))
                                            .collect::<Vec<_>>(),
                                        return_type,
                                    }),
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

            ItemDef::Impl(ImplBlockDef {
                generics: rust_impl_block_generics,
                trait_: trait_index,
                target: target_rust_type.clone(),
                rust_items,
                syn: item_impl.clone(),
            })
        }
        ItemDefNoTypes::None => panic!(),
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
    impl_generics: &[(String, Vec<usize>)],
    // NOTE this will simply be empty for items that can't be generic, ie consts, or can but simply don't have any
    root_parent_item_def_generics: &[(String, Vec<usize>)],
    // TODO should just store the current module in GlobalData to save having to pass this around everywhere
    current_module: &[String],
    // global_data: &make_item_definitions::GlobalData,
    crates: &[RustMod],
    item_defs: &[ItemDefNoTypes],
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
                                                    impl_generics,
                                                    root_parent_item_def_generics,
                                                    current_module,
                                                    crates,
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
                                                    impl_generics,
                                                    root_parent_item_def_generics,
                                                    current_module,
                                                    crates,
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
                                                        impl_generics,
                                                        root_parent_item_def_generics,
                                                        current_module,
                                                        crates,
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
                                    crates,
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
            let parent_generic = root_parent_item_def_generics
                .iter()
                .find(|(generic_name, _trait_bounds)| generic_name == seg_name_str);

            let impl_block_generic = impl_generics
                .iter()
                .find(|(generic_name, _trait_bounds)| generic_name == seg_name_str);

            if let Some((generic_name, trait_bounds)) = parent_generic.or(impl_block_generic) {
                // return match &generic.type_ {
                //     RustTypeParamValue::Unresolved => todo!(),
                //     RustTypeParamValue::RustType(rust_type) => *rust_type.clone(),
                // };
                return RustType::TypeParam(RustTypeParam {
                    name: generic_name.clone(),
                    trait_bounds: trait_bounds.clone(),
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
                    // let generic_type = match &seg.arguments {
                    //     PathArguments::AngleBracketed(angle_bracketed_generic_arguments) => {
                    //         // Option only has
                    //         match angle_bracketed_generic_arguments.args.first().unwrap() {
                    //             GenericArgument::Lifetime(_) => todo!(),
                    //             GenericArgument::Type(type_) => {
                    //                 parse_types_for_populate_item_definitions(
                    //                     type_,
                    //                     root_parent_item_definition_generics,
                    //                     current_module,
                    //                     crates,
                    //                     item_defs,
                    //                     scoped_items,
                    //                 )
                    //             }
                    //             GenericArgument::Const(_) => todo!(),
                    //             GenericArgument::AssocType(_) => todo!(),
                    //             GenericArgument::AssocConst(_) => todo!(),
                    //             GenericArgument::Constraint(_) => todo!(),
                    //             _ => todo!(),
                    //         }
                    //     }
                    //     _ => todo!(),
                    // };
                    // RustType::Option(RustTypeParam {
                    //     // TODO "T" shouldn't be hardcoded here
                    //     name: "T".to_string(),
                    //     trait_bounds: todo!(),
                    //     type_: RustTypeParamValue::RustType(Box::new(generic_type)),
                    // })
                }
                // "Result" => {
                // todo!();
                // let generic_type = match &seg.arguments {
                //     PathArguments::AngleBracketed(angle_bracketed_generic_arguments) => {
                //         // Option only has
                //         match angle_bracketed_generic_arguments.args.first().unwrap() {
                //             GenericArgument::Lifetime(_) => todo!(),
                //             GenericArgument::Type(type_) => {
                //                 parse_types_for_populate_item_definitions(
                //                     type_,
                //                     root_parent_item_definition_generics,
                //                     current_module,
                //                     crates,
                //                     item_defs,
                //                     scoped_items,
                //                 )
                //             }
                //             GenericArgument::Const(_) => todo!(),
                //             GenericArgument::AssocType(_) => todo!(),
                //             GenericArgument::AssocConst(_) => todo!(),
                //             GenericArgument::Constraint(_) => todo!(),
                //             _ => todo!(),
                //         }
                //     }
                //     _ => todo!(),
                // };
                // RustType::Result(RustTypeParam {
                //     // TODO "T" shouldn't be hardcoded here
                //     name: "T".to_string(),
                //     type_: RustTypeParamValue::RustType(Box::new(generic_type)),
                // })
                // }
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
                                                impl_generics,
                                                root_parent_item_def_generics,
                                                current_module,
                                                crates,
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
                            crates,
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
                            // TODO
                            trait_bounds: vec![],
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
                        } else if item_seg.ident == "Result" {
                            assert_eq!(type_params.len(), 2);
                            RustType::Result(type_params.remove(0), type_params.remove(0))
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
                impl_generics,
                root_parent_item_def_generics,
                current_module,
                crates,
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

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone)]
pub enum StructFieldInfo {
    UnitStruct,
    TupleStruct(Vec<RustType>),
    /// (name, type)
    RegularStruct(Vec<(String, RustType)>),
}

#[derive(Debug, Clone)]
pub struct StructDefUniqueInfo {
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
pub struct EnumDefUniqueInfo {
    pub members: Vec<EnumVariantInfo>,
    pub syn_object: ItemEnum,
}

#[derive(Debug, Clone)]
pub enum StructEnumUniqueInfo2 {
    Struct(StructDefUniqueInfo),
    Enum(EnumDefUniqueInfo),
}

/// Similar to StructOrEnum which gets used in RustType, but is for storing info about the actual item definition, rather than instances of, so eg we don't need to be able to store resolved generics. Minor differences but making distinct type helps with reasoning about the different use cases.
/// Just structs and enums or should we include functions?
#[derive(Debug, Clone)]
pub struct StructEnumDef {
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
    /// (type param name, trait bounds)
    pub generics: Vec<(String, Vec<usize>)>,
    // syn_object: StructOrEnumSynObject,
    pub struct_or_enum_info: StructEnumUniqueInfo2,
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
    // Store indexes of traits with any default impls, that the struct impls
    pub traits: Vec<usize>,
}
impl StructEnumDef {
    pub fn get_type(&self, field_member: &Member, global_data: &GlobalData) -> RustType2 {
        match &self.struct_or_enum_info {
            StructEnumUniqueInfo2::Struct(struct_def_info) => match &struct_def_info.fields {
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
            StructEnumUniqueInfo2::Enum(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub js_name: Ident,
    pub ident: String,
    pub is_pub: bool,
    pub items: Vec<TraitItemDef>,
    pub syn: ItemTrait,
    pub default_impls: Vec<RustImplItemNoJs>,
}
#[derive(Debug, Clone)]
pub enum TraitItemDef {
    Fn(Rc<FnSigDef>),
    Const,
    Type,
}

// Do we want to also store the trait bounds of each type param? This way if we have an unresolved type param that calls some function, we will know what trait to look up to find it. In some cases this might also remove the need for looking forward to resolve type params, if all we need to do with the type param/value/intance/type is call a method on it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustTypeParam {
    pub name: String,
    // TODO What if the trait is generic and so has concrete types specified? Need something like `Vec<(RustTypeParam, usize)>`?
    pub trait_bounds: Vec<usize>,
    pub type_: RustTypeParamValue,
}
impl RustTypeParam {
    pub fn into_rust_type_param2(self, global_data: &GlobalData) -> RustTypeParam2 {
        RustTypeParam2 {
            name: self.name,
            trait_bounds: self
                .trait_bounds
                .into_iter()
                .map(|index| match &global_data.item_defs[index] {
                    ItemDefRc::Trait(trait_def) => trait_def.clone(),
                    _ => todo!(),
                })
                .collect(),
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
    Fn(bool, FnDef),
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
    /// We need Self for the `&self` input in default trait impl methods, since we won't know the concrete type while parsing the fn body.
    /// Surely this is the same for `impl Foo for T {}` trait impls? Kind of, it will be an unresolved RustType::TypeParam.
    Self_,
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
    Result(RustTypeParam, RustTypeParam),
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
            RustType::Result(ok_type_param, err_type_param) => RustType2::Result(
                ok_type_param.into_rust_type_param2(global_data),
                err_type_param.into_rust_type_param2(global_data),
            ),
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
                    // Impl block fns are not stored simply as Rc fns in the item defs Vec, there are only store within the impl, yet here we are trying to represent them as a RustType2::Fn
                    fn_info.sig.clone(),
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
            RustType::Self_ => RustType2::Self_,
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
    pub ident: String,
    pub is_pub: bool,
    pub type_: RustType,
    pub expr: ExprRef,
}

/// Not just for methods, can also be an enum variant with no inputs
#[derive(Debug, Clone)]
pub struct FnDef {
    pub is_pub: bool,
    // TODO No point storing all the info like inputs and return types separately, as these need to be stored on RustType::Fn anyway for eg closures where we won't be storing a fn info?? Keep both for now and revisit later. Note fns idents can just appear in the code and be called whereas a closure will be a var which already has a type.
    // pub js_name: Ident,
    // pub ident: String,
    /// Does this include receiver/self types? NO in handle_item_fn we are filtering out any self type. Could just store it as RustType::Self, but seems pointless if we don't actually need it for anything. NO again, updated to include self inputs because we need them.
    /// TODO probably don't actually need `is_self`
    /// (is_self, is_mut, name, type)
    // pub inputs_types: Vec<(bool, bool, String, RustType)>,
    /// (type param name, trait bounds)
    // pub generics: Vec<(String, Vec<usize>)>,
    // pub generics: Vec<String>,
    // NO! for methods we want to store the actual fn type. fns can be assigned to vars, and we want to be able to pass the Path part of the fn, and *then* call it and determine the return type
    // pub return_type: RustType,
    // /// type of fn eg Fn(i32) -> ()
    // rust_type: RustType,
    // TODO optionally add enum for Field, AssociatedFn, Method, etc
    pub syn: FnInfoSyn,

    pub stmts: Vec<StmtsRef>,
    pub sig: Rc<FnSigDef>,
}
#[derive(Debug, Clone)]
pub struct FnSigDef {
    pub js_name: Ident,
    pub ident: String,
    /// (is_self, is_mut, name, type)
    pub inputs_types: Vec<(bool, bool, String, RustType)>,
    /// (type param name, used directly like `T::associated_fn()`, trait bounds)
    pub generics: Vec<(String, bool, Vec<usize>)>,
    // pub generics: Vec<String>,
    // NO! for methods we want to store the actual fn type. fns can be assigned to vars, and we want to be able to pass the Path part of the fn, and *then* call it and determine the return type
    pub return_type: RustType,
}

impl FnSigDef {
    pub fn attempt_to_resolve_type_params_using_arg_types(
        &self,
        args: &[RustType2],
        item_defs: &[ItemDefRc],
    ) -> Vec<RustTypeParam2> {
        self.generics
            .iter()
            .map(|(type_param_name, _used_associated_fn, trait_bounds)| {
                let matched_arg_rust_type = self.inputs_types.iter().enumerate().find_map(
                    |(i, (_is_self, _is_mut, _name, input_type))| {
                        match input_type {
                            RustType::TypeParam(type_param)
                                if type_param_name == &type_param.name =>
                            {
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

                let trait_bounds = trait_bounds
                    .iter()
                    .map(|index| match &item_defs[*index] {
                        ItemDefRc::Trait(trait_def) => trait_def.clone(),
                        _ => todo!(),
                    })
                    .collect::<Vec<_>>();

                RustTypeParam2 {
                    name: type_param_name.clone(),
                    trait_bounds,
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
pub struct ImplBlockDef {
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
