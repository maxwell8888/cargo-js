use heck::AsPascalCase;
use std::fs;
use syn::{
    Fields, FnArg, GenericParam, ImplItemFn, ItemMod, Meta, Pat, ReturnType, Type, Visibility,
};
use tracing::{debug, debug_span, info};

use super::{
    definition_data::{GlobalDataScope, JsImplBlock2, RustImplItemJs, ScopedVar},
    handle_syn_expr::handle_expr,
    handle_syn_stmt::parse_fn_body_stmts,
};

use crate::{
    js_ast::{
        Ident, JsClass, JsExpr, JsFn, JsLocal, JsModule, JsStmt, LocalName, LocalType, PathIdent,
    },
    make_item_definitions::{FnInfoSyn, ItemRef, StmtsRef},
    update_item_definitions::{
        ItemDef, RustImplItemItemNoJs, RustImplItemNoJs, RustTypeParam, RustTypeParamValue,
        StructOrEnumDefitionInfo2,
    },
    GlobalData, RustImplItemItemJs, RustType2, PRELUDE_MODULE_PATH,
};

// TODO remove this as it is unnecessary redirection
/// Converts a Vec<syn::Item> to Vec<JsStmt> and moves method impls into their class
///
/// all users (eg crate, fn, file) want to group classes, but only crates want to populate boilerplate
pub fn js_stmts_from_syn_items(
    // items: Vec<Item>,
    module_item_refs: &[ItemRef],
    // Need to keep track of which module we are currently in, for constructing the boilerplate
    current_module: &[String],
    global_data: &mut GlobalData,
    // ) -> Vec<JsStmt> {
    // (stmts, sub modules)
) -> (Vec<JsStmt>, Vec<JsModule>) {
    let span = debug_span!("js_stmts_from_syn_items", current_module = ?current_module);
    let _guard = span.enter();

    let mut js_stmts = Vec::new();
    // let mut modules = Vec::new();
    // TODO this should be optional/configurable, might not always want it

    // We need to know what the syn type is to know whether it is eg a use which needs adding to the boiler plate
    // but we also need to put the struct impls into the class
    // solution:
    // push use to boilerplate in handle_item() and don't push the item to js_stmts
    // push other items as normal
    // after loop, group together classes
    // now that classes are grouped, we can add them to the module object - we either do the conversion to module objects right at the end after parsing all branches, otherwise we are always going to have to be able to amend the classes because impls might be in other modules.

    // remember that `impl Foo` can appear before `struct Foo {}` so classes definitely need multiple passes or to init class when we come across an impl, and then place it and add other data when we reach the actual struct definition
    // What happens when a method impl is outside the class's module? Could just find the original class and add it, but what if the method if using items from *it's* module? Need to replace the usual `this.someItem` with eg `super.someItem` or `subModule.someItem`. So we need to be able to find classes that appear in other modules
    // dbg!("js_stmts_from_syn_items");
    // dbg!(&global_data.scope_id);
    let item_defs = global_data.item_defs.clone();
    let mut temp_submodules = Vec::new();

    // for item in &global_data.item_refs_to_render.clone() {
    for item in module_item_refs {
        // handle_item(item, global_data, current_module, &mut js_stmts);
        match item {
            ItemRef::Const(index) => {
                let js_stmt = handle_item_const(*index, true, global_data, current_module);
                js_stmts.push(js_stmt);
            }
            ItemRef::StructOrEnum(index) => {
                let item = &item_defs[*index];
                match item {
                    ItemDef::StructEnum(actual) => match &actual.struct_or_enum_info {
                        StructOrEnumDefitionInfo2::Struct(_struct_def) => {
                            js_stmts.push(handle_item_struct(
                                *index,
                                true,
                                global_data,
                                current_module,
                            ));
                        }
                        StructOrEnumDefitionInfo2::Enum(_enum_def) => {
                            js_stmts.push(handle_item_enum(
                                *index,
                                true,
                                global_data,
                                current_module,
                            ));
                        }
                    },
                    _ => todo!(),
                }
            }
            // Item::ExternCrate(_) => todo!(),
            ItemRef::Fn(index) => {
                js_stmts.push(handle_item_fn(*index, true, global_data, current_module));
            }
            // Item::ForeignMod(_) => todo!(),
            ItemRef::Impl(index) => {
                // TODO maybe it would be better for handle_item_impl (and similar fns) to return a JsClass and then we wrap it into a stmt here?
                js_stmts.extend(handle_item_impl(*index, true, global_data, current_module));
            }
            // ItemRef::Macro(_) => todo!(),
            ItemRef::Mod(rust_mod) => {
                // NOTE in contrast to the other handlers here, handle_item_mod actually mutates `current_module_path` and appends a new JsModule to `global_data.transpiled_modules` instead of appending statements to `js_stmts`
                // handle_item_mod(item_mod, global_data, current_module)

                let (stmts, mut submodules) =
                    js_stmts_from_syn_items(&rust_mod.items, &rust_mod.module_path, global_data);
                temp_submodules.push(JsModule {
                    public: true,
                    name: Ident::String(rust_mod.module_path.last().unwrap().clone()),
                    module_path: rust_mod.module_path.clone(),
                    stmts,
                });
                temp_submodules.append(&mut submodules);
            }
            // ItemRef::Static(_) => todo!(),
            ItemRef::Trait(index) => {
                handle_item_trait(*index, true, global_data, current_module);
                js_stmts.push(JsStmt::Expr(JsExpr::Vanish, false));
            }
            // Item::TraitAlias(_) => todo!(),
            // Item::Type(_) => todo!(),
            // Item::Union(_) => todo!(),
            ItemRef::Use(_item_use) => {
                //
                // handle_item_use(&item_use, ItemUseModuleOrScope::Module(module));
            }
            // Item::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }

    (js_stmts, temp_submodules)
}

pub fn handle_item_fn(
    // fn_info: &FnInfo,
    index: usize,
    // For keeping track of whether we are parsing items at the module level or in a fn scope, so that we know whether we need to add the items to `.scopes` or not.
    // Good also keep track using a field on global data, but for now seems less error prone to pass values to handle fns because it is always clear whether we are at the top level based on whether the item is being parsed within `handle_statments()`
    _at_module_top_level: bool,
    global_data: &mut GlobalData,
    current_module: &[String],
) -> JsStmt {
    let fn_info = match &global_data.item_defs[index] {
        ItemDef::Fn(fn_info) => fn_info.clone(),
        other => {
            dbg!(other);
            todo!()
        }
    };

    let item_fn = match &fn_info.syn {
        FnInfoSyn::Standalone(item_fn) => item_fn,
        FnInfoSyn::Impl(_) => todo!(),
        FnInfoSyn::Trait(_) => todo!(),
    };

    let name = item_fn.sig.ident.to_string();
    let span = debug_span!("handle_item_fn", name = ?name);
    let _guard = span.enter();

    let ignore = if let Some(thing) = item_fn.attrs.first() {
        match &thing.meta {
            Meta::Path(path) => {
                if let Some(seg) = path.segments.first() {
                    seg.ident == "ignore"
                } else {
                    false
                }
            }
            _ => false,
        }
    } else {
        false
    };

    // // NOTE we only push scoped definitions because module level definition are already pushed in extract_data_populate_item_definitions
    // // if !global_data.at_module_top_level {
    // if !at_module_top_level {
    //     // Record this fn in the *parent* scope

    //     // let fn_info = FnInfo {
    //     //     ident: name,
    //     //     rust_type: match &item_fn.sig.output {
    //     //         ReturnType::Default => RustType::Fn((), RustType::Unit,),
    //     //         ReturnType::Type(_, type_) => {
    //     //             parse_fn_input_or_field(&*type_, &Vec::new(), current_module, &global_data)
    //     //         }
    //     //     },
    //     // };

    //     global_data.scopes.last_mut().unwrap().fns.push(fn_info);
    // }

    // Because item definitions can appear after they are used, we can't simply add them to the scope as they are handled. We instead need to go through all the stmts and record the defined items before we do a second pass to actually handle/parse the stmts.
    let scoped_item_defs = fn_info
        .stmts
        .iter()
        .filter_map(|stmt_ref| {
            match stmt_ref {
                StmtsRef::Item(item_ref) => {
                    match item_ref {
                        ItemRef::StructOrEnum(index) => Some(*index),
                        ItemRef::Fn(index) => Some(*index),
                        // TODO can consts be scoped or are they always global??
                        ItemRef::Const(index) => Some(*index),
                        ItemRef::Trait(index) => Some(*index),
                        ItemRef::Mod(_) => todo!(),
                        ItemRef::Use(_) => todo!(),
                        ItemRef::Macro => todo!(),
                        ItemRef::Impl(_) => None,
                    }
                }
                _ => None,
            }
        })
        .collect();

    // Create new scope for fn vars
    global_data.scopes.push(GlobalDataScope {
        variables: Vec::new(),
        items: scoped_item_defs,
        _look_in_outer_scope: false,
        use_mappings: Vec::new(),
    });

    // Adds fn args as `ScopedVar`s
    // Adds intial lines needs for copy types like `let fn_arg = fn_arg.copy();`
    let mut copy_stmts = Vec::new();

    for (_is_self, is_mut, name, type_) in &fn_info.inputs_types {
        let type_ = type_.clone().into_rust_type2(global_data);

        let scoped_var = ScopedVar {
            name: name.clone(),
            mut_: *is_mut,
            type_: type_.clone(),
        };
        // record add var to scope
        global_data
            .scopes
            .last_mut()
            .unwrap()
            .variables
            .push(scoped_var);

        // a mut input of a copy type like `mut num: i32` must be converted to `RustInteger`
        // TODO need to add this to `handle_impl_fn_item()`
        if *is_mut {
            copy_stmts.push(JsStmt::Local(JsLocal {
                public: false,
                export: false,
                type_: LocalType::None,
                lhs: LocalName::Single(Ident::String(name.clone())),
                value: match type_ {
                    RustType2::NotAllowed => todo!(),
                    RustType2::Unknown => todo!(),
                    RustType2::Todo => todo!(),
                    RustType2::Unit => todo!(),
                    RustType2::Never => todo!(),
                    RustType2::ImplTrait(_) => todo!(),
                    RustType2::TypeParam(_) => todo!(),
                    RustType2::I32 => {
                        global_data.rust_prelude_types.rust_integer = true;
                        // JsExpr::New(
                        //     vec!["RustInteger".to_string()],
                        //     vec![JsExpr::MethodCall(
                        //         Box::new(JsExpr::Path(vec![pat_ident
                        //             .ident
                        //             .to_string()])),
                        //         "inner".to_string(),
                        //         Vec::new(),
                        //     )],
                        // )
                        JsExpr::New(
                            "RustInteger".into(),
                            vec![JsExpr::Path(PathIdent::Single(Ident::String(name.clone())))],
                        )
                    }
                    RustType2::F32 => todo!(),
                    RustType2::Bool => todo!(),
                    RustType2::String => todo!(),
                    RustType2::Option(_) => todo!(),
                    RustType2::Result(_) => todo!(),
                    RustType2::StructOrEnum(_, _) => todo!(),
                    RustType2::Vec(_) => todo!(),
                    RustType2::Array(_) => todo!(),
                    RustType2::Tuple(_) => todo!(),
                    RustType2::Box(_) => todo!(),
                    RustType2::UserType(_, _) => todo!(),
                    RustType2::MutRef(_) => todo!(),
                    RustType2::Ref(_) => todo!(),
                    RustType2::Fn(_, _, _) => todo!(),
                    RustType2::FnVanish => todo!(),
                    RustType2::Closure(_, _) => todo!(),
                },
            }))
        }
    }

    let stmt = if ignore {
        JsStmt::Expr(JsExpr::Vanish, false)
    } else {
        // If we are returning a type which is *not* &mut, then we need to `.copy()` or `.inner()` if the value being returned is mut (if the value is &mut, the compiler will have ensured there is a deref, so we will have already added a `.copy()` or `.inner()`).
        let returns_non_mut_ref_val = match &item_fn.sig.output {
            ReturnType::Default => true,
            ReturnType::Type(_, type_) => {
                let is_mut_ref = matches!(&**type_, Type::Reference(type_reference) if type_reference.mutability.is_some());
                !is_mut_ref
            }
        };
        // dbg!(&item_fn.block.stmts);

        let (body_stmts, _return_type) = parse_fn_body_stmts(
            false,
            returns_non_mut_ref_val,
            true,
            &fn_info.stmts,
            global_data,
            current_module,
        );

        copy_stmts.extend(body_stmts);
        // let iife = item_fn.sig.ident == "main";
        // wrapping main in an iffe means we need to move it to the end of the JS file, given we have the crate module appear first, and even if it appears last, main() can appear anywhere within the crate module so the easist thing is to just append a `main();` call to the end of the JS file
        let iife = false;
        let js_fn = JsFn {
            iife,
            // iife: false,
            public: match item_fn.vis {
                Visibility::Public(_) => true,
                Visibility::Restricted(_) => todo!(),
                Visibility::Inherited => false,
            },
            export: false,
            async_: item_fn.sig.asyncness.is_some(),
            is_method: false,
            name: fn_info.js_name.clone(),
            input_names: item_fn
                .sig
                .inputs
                .iter()
                .map(|input| match input {
                    FnArg::Receiver(_) => todo!(),
                    FnArg::Typed(pat_type) => match &*pat_type.pat {
                        Pat::Ident(pat_ident) => Ident::Syn(pat_ident.ident.clone()),
                        _ => todo!(),
                    },
                })
                .collect::<Vec<_>>(),
            body_stmts: copy_stmts,
        };
        if iife {
            JsStmt::Expr(JsExpr::Fn(js_fn), true)
        } else {
            JsStmt::Function(js_fn)
        }
        // JsStmt::Function(js_fn)
    };

    // pop fn scope
    global_data.scopes.pop();

    stmt
}

pub fn handle_item_const(
    index: usize,
    _at_module_top_level: bool,
    global_data: &mut GlobalData,
    current_module: &[String],
) -> JsStmt {
    // let mut name = Ident::Syn(item_const.ident.clone());
    // debug!(name = ?name, "handle_item_const");

    let const_def = match &global_data.item_defs[index] {
        ItemDef::Const(const_def) => const_def.clone(),
        _ => todo!(),
    };

    JsStmt::Local(JsLocal {
        export: false,
        public: const_def.is_pub,
        type_: LocalType::Const,
        lhs: LocalName::Single(const_def.js_name.clone()),
        value: handle_expr(&const_def.expr, global_data, current_module).0,
    })
}

/// We convert enum variants like Foo::Bar to Foo.bar because otherwise when the variant has arguments, syn is not able to distinguish it from an associated method, so we cannot deduce when Pascal or Camel case should be used, so stick to Pascal for both case.
/// We must store separate <variant name>Id fields because otherwise we end up in a situation where a variable containing an enum variant only contains the data returned the the method with that name and then we can't do myVariantVar === MyEnum::Variant because the lhs is data and the rhs is a function.
pub fn handle_item_enum(
    index: usize,
    _at_module_top_level: bool,
    global_data: &mut GlobalData,
    current_module: &[String],
) -> JsStmt {
    let item = global_data.item_defs[index].clone();
    let item_def = match item {
        ItemDef::StructEnum(item_def) => item_def,
        _ => todo!(),
    };

    let item_enum = match &item_def.struct_or_enum_info {
        StructOrEnumDefitionInfo2::Struct(_) => todo!(),
        StructOrEnumDefitionInfo2::Enum(enum_def_info) => &enum_def_info.syn_object,
    };

    let enum_name = item_enum.ident.to_string();
    debug!(enum_name = ?enum_name, "handle_item_enum");
    // dbg!(item_enum.attrs);

    // Keep track of structs/enums in scope so we can subsequently add impl'd methods and then look up their return types when the method is called

    // TODO we can't just get the type of a variant from the enum definiton if it is generic, as the generic type info will only be available in the actual code where that particular instance of the enum is instantiated, or later on. Well actually we will be looking this up from where the enum is instantiated like `MyEnum::MyVariant` so maybe just return the type if there is no enum, else return something to flag that the generic type should be looked for??
    let _generics = item_enum
        .generics
        .params
        .iter()
        .map(|p| match p {
            GenericParam::Lifetime(_) => todo!(),
            GenericParam::Type(type_param) => RustTypeParam {
                name: type_param.ident.to_string(),
                type_: RustTypeParamValue::Unresolved,
            },
            GenericParam::Const(_) => todo!(),
        })
        .collect::<Vec<_>>();

    // let return_type_for_scope = RustType::Enum(ItemDefinition {
    //     ident: item_enum.ident.to_string(),
    //     members: item_enum
    //         .variants
    //         .iter()
    //         .map(|v| MemberInfo {
    //             ident: v.ident.to_string(),
    //             return_type: RustType::ParentItem,
    //         })
    //         .collect::<Vec<_>>(),
    //     generics,
    //     syn_object: StructOrEnumSynObject::Enum(item_enum.clone()),
    // });

    let _class_name = item_enum.ident.to_string();

    // Populate methods and fields

    let mut static_fields = Vec::new();
    for variant in &item_enum.variants {
        static_fields.push(JsLocal {
            public: false,
            export: false,
            type_: LocalType::Static,
            lhs: LocalName::Single(Ident::String(format!("{}Id", variant.ident))),
            value: JsExpr::LitStr(variant.ident.to_string()),
        });

        match variant.fields {
            syn::Fields::Named(_) => {}
            syn::Fields::Unnamed(_) => {}
            syn::Fields::Unit => {
                static_fields.push(JsLocal {
                    public: false,
                    export: false,
                    type_: LocalType::Static,
                    lhs: LocalName::Single(Ident::String(
                        AsPascalCase(variant.ident.to_string()).to_string(),
                    )),
                    value: JsExpr::New(
                        item_enum.ident.clone().into(),
                        vec![JsExpr::LitStr(variant.ident.to_string()), JsExpr::Null],
                    ),
                    // Box::new(JsExpr::LitStr(variant.ident.to_string())),
                });
            }
        };
    }

    // let mut class_name = Ident::Syn(item_enum.ident.clone());

    let mut methods = Vec::new();
    let body_stmts = vec![
        JsStmt::Raw("this.id = id;".to_string()),
        JsStmt::Raw("this.data = data;".to_string()),
    ];

    // TODO using syn types like JsImplItem {impl_item: ImplItem, body_stmts: Vec<JsStmt>} is no good if you want to manually construct eg a method! Unless we had two version ie a syn type version and a manual version, using either an enum or trait objects.
    methods.push((
        Ident::Syn(item_enum.ident.clone()),
        false,
        JsFn {
            iife: false,
            public: false,
            export: false,
            async_: false,
            is_method: true,
            name: Ident::Str("constructor"),
            input_names: vec![Ident::Str("id"), Ident::Str("data")],
            body_stmts,
        },
    ));

    let mut static_fields = Vec::new();
    for variant in &item_enum.variants {
        static_fields.push(JsLocal {
            public: false,
            export: false,
            type_: LocalType::Static,
            lhs: LocalName::Single(Ident::String(format!("{}Id", variant.ident))),
            value: JsExpr::LitStr(variant.ident.to_string()),
        });

        match variant.fields {
            syn::Fields::Named(_) => {}
            syn::Fields::Unnamed(_) => {}
            syn::Fields::Unit => {
                static_fields.push(JsLocal {
                    public: false,
                    export: false,
                    type_: LocalType::Static,
                    lhs: LocalName::Single(Ident::String(
                        AsPascalCase(variant.ident.to_string()).to_string(),
                    )),
                    value: JsExpr::New(
                        // item_enum.ident.clone().into(),
                        PathIdent::Single(item_def.js_name.clone()),
                        vec![JsExpr::LitStr(variant.ident.to_string()), JsExpr::Null],
                    ),
                    // Box::new(JsExpr::LitStr(variant.ident.to_string())),
                });
            }
        };
    }

    for variant in &item_enum.variants {
        let (input_names, body_stmts) = match &variant.fields {
            syn::Fields::Named(_fields_named) => {
                let stmt = JsStmt::Expr(
                    JsExpr::Return(Box::new(JsExpr::New(
                        item_enum.ident.clone().into(),
                        vec![
                            JsExpr::LitStr(variant.ident.to_string()),
                            JsExpr::Var("data".to_string()),
                        ],
                    ))),
                    true,
                );
                (vec![Ident::Str("data")], vec![stmt])
            }
            syn::Fields::Unnamed(fields_unnamed) => {
                let arg_names = fields_unnamed
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, _)| Ident::String(format!("arg{i}")))
                    .collect::<Vec<_>>();

                let return_expr = JsExpr::Return(Box::new(JsExpr::New(
                    item_enum.ident.clone().into(),
                    vec![
                        JsExpr::LitStr(variant.ident.to_string()),
                        JsExpr::Array(
                            arg_names
                                .iter()
                                .map(|name| JsExpr::Path(PathIdent::Single(name.clone())))
                                .collect::<Vec<_>>(),
                        ),
                    ],
                )));
                (arg_names, vec![JsStmt::Expr(return_expr, true)])
            }
            syn::Fields::Unit => (Vec::new(), Vec::new()),
        };
        if !body_stmts.is_empty() {
            methods.push((
                Ident::Syn(item_enum.ident.clone()),
                true,
                JsFn {
                    iife: false,
                    public: false,
                    export: false,
                    async_: false,
                    is_method: true,
                    name: Ident::Syn(variant.ident.clone()),
                    input_names,
                    body_stmts,
                },
            ))
        }
    }

    let mut js_class = JsClass {
        public: match item_enum.vis {
            Visibility::Public(_) => true,
            Visibility::Restricted(_) => todo!(),
            Visibility::Inherited => false,
        },
        export: false,
        tuple_struct: false,
        name: item_def.js_name.clone(),
        inputs: Vec::new(),
        static_fields,
        methods,
        rust_name: item_enum.ident.to_string(),
        is_impl_block: false,
        module_path: current_module.to_vec(),
        scope_id: None,
        // struct_or_enum: StructOrEnumSynObject::Enum(item_enum.clone()),
        // impld_methods: methods,
        // generic_trait_impl_methods: todo!(),
    };

    let mut dedup_impl_block_ids = item_def.impl_block_ids.clone();
    dedup_impl_block_ids.sort();
    dedup_impl_block_ids.dedup();
    for impl_block_id in &dedup_impl_block_ids {
        // TODO IMPORTANT we are parsing impl block methods below when it is also being done in handle_item_impl(), this probably breaks assumptions eg mutates data twice etc

        let rust_impl_block = match global_data.item_defs[*impl_block_id].clone() {
            ItemDef::Impl(impl_block) => impl_block,
            other => {
                dbg!(index);
                dbg!(other);
                todo!()
            }
        };

        let target_rust_type = rust_impl_block.target.into_rust_type2(global_data);

        global_data
            .impl_block_target_type
            .push(target_rust_type.clone());

        let rust_impl_items = rust_impl_block
            .rust_items
            .into_iter()
            .map(|item| RustImplItemJs {
                ident: item.ident.clone(),
                item: match &item.item {
                    RustImplItemItemNoJs::Fn(_static_, fn_info) => {
                        // TODO IMPORTANT reuse code from handle_item_fn

                        // RustImplItemItemJs::Fn(static_, fn_info, js)
                        handle_impl_item_fn(
                            // impl_item,
                            match &fn_info.syn {
                                FnInfoSyn::Standalone(_) => todo!(),
                                FnInfoSyn::Impl(impl_item_fn) => impl_item_fn,
                                FnInfoSyn::Trait(_) => todo!(),
                            },
                            global_data,
                            current_module,
                            &target_rust_type,
                            &item,
                        )
                    }
                    RustImplItemItemNoJs::Const => RustImplItemItemJs::Const(todo!()),
                },
            })
            .collect::<Vec<_>>();

        let js_impl_block = JsImplBlock2 {
            _index: index,
            _generics: rust_impl_block.generics,
            trait_: rust_impl_block.trait_,
            target: target_rust_type.clone(),
            items: rust_impl_items
                .into_iter()
                .map(|x| (false, x))
                .collect::<Vec<_>>(),
        };

        let is_generic_impl = matches!(js_impl_block.target, RustType2::TypeParam(_));

        for (_used, impl_item) in &js_impl_block.items {
            // TODO implement used
            // TODO What about `impl Foo for T {}`? This means we need to add prototype fields, not methods?
            match &impl_item.item {
                RustImplItemItemJs::Fn(static_, _fn_info, js_fn) => {
                    if is_generic_impl {
                        js_class.static_fields.push(JsLocal {
                            public: false,
                            export: false,
                            type_: LocalType::None,
                            lhs: LocalName::Single(js_fn.name.clone()),
                            value: JsExpr::Path(PathIdent::Path(
                                [
                                    js_impl_block.js_name(),
                                    Ident::Str("prototype"),
                                    js_fn.name.clone(),
                                ]
                                .to_vec(),
                            )),
                        });
                    } else {
                        js_class.methods.push((
                            Ident::String(item_def.ident.clone()),
                            *static_,
                            js_fn.clone(),
                        ));
                    }
                }
                RustImplItemItemJs::Const(_) => todo!(),
            }
        }
    }
    JsStmt::Class(js_class)
}

pub fn handle_impl_item_fn(
    impl_item_fn: &ImplItemFn,
    global_data: &mut GlobalData,
    current_module_path: &[String],
    target_rust_type: &RustType2,
    rust_impl_item: &RustImplItemNoJs,
) -> RustImplItemItemJs {
    let _static_ = matches!(impl_item_fn.sig.inputs.first(), Some(FnArg::Receiver(_)));
    // let private = !export;
    let js_input_names = impl_item_fn
        .clone()
        .sig
        .inputs
        .into_iter()
        .filter_map(|input| match input {
            FnArg::Receiver(_) => None,
            FnArg::Typed(pat_type) => match *pat_type.pat {
                Pat::Ident(pat_ident) => Some(Ident::Syn(pat_ident.ident.clone())),
                _ => todo!(),
            },
        })
        .collect::<Vec<_>>();

    // update generics with any `impl Fn... -> ...` types defined in where clauses
    // let where_clause = &item_impl_fn.sig.generics.where_clause;
    // if let Some(where_clause) = where_clause {
    //     for where_predicate in &where_clause.predicates {
    //         match where_predicate {
    //             WherePredicate::Lifetime(_) => todo!(),
    //             WherePredicate::Type(predicate_type) => {
    //                 let name = match &predicate_type.bounded_ty {
    //                     Type::Path(type_path) => {
    //                         type_path.path.segments.first().unwrap().ident.to_string()
    //                     }
    //                     _ => todo!(),
    //                 };
    //                 let type_param_bound = predicate_type.bounds.first().unwrap();
    //                 let type_ = get_return_type_of_type_param_bound(
    //                     type_param_bound,
    //                     &fn_generics,
    //                     current_module_path,
    //                     &global_data,
    //                 );
    //                 // MyGeneric { name, type_ }
    //                 let generic = fn_generics
    //                     .iter_mut()
    //                     .find(|my_generic| my_generic.name == name)
    //                     .unwrap();
    //                 generic.type_ = type_;
    //             }
    //             _ => todo!(),
    //         }
    //     }
    // }

    // let where_generics = item_impl_fn
    //     .sig
    //     .generics
    //     .where_clause
    //     .as_ref()
    //     .map(|where_clause| {
    //         where_clause
    //             .predicates
    //             .iter()
    //             .map(|where_predicate| )
    //             .collect::<Vec<_>>()
    //     })
    //     .unwrap_or(Vec::new());
    // dbg!(&where_generics);
    // generics.extend(where_generics);

    let mut vars = Vec::new();
    match &rust_impl_item.item {
        RustImplItemItemNoJs::Fn(_static, fn_info) => {
            for (is_self, is_mut, name, input_type) in fn_info.inputs_types.clone() {
                if is_self {
                    // TODO we need to ensure that RustType::Parent type is getting wrapped in RustType::MutRef where necessary

                    // NOTE THIS IS WRONG, we parsing the definition so the type params won't have changed from the first pass. It is once the method is *called* that we can attempt to further resolve type params eg the `T` in an `Option<T>`
                    // Also, shouldn't need this, the type should already be wrapped in `RustType::MutRef` if it is `&mut self`
                    let _type_ = if is_mut {
                        // TODO does this mean self in `fn foo(mut self) {}` goes to RustType::MutRef??
                        RustType2::MutRef(Box::new(target_rust_type.clone()))
                    } else {
                        target_rust_type.clone()
                    };

                    // TODO why can't self have `mut self`??? write tests.
                    assert!(!is_mut);
                    let scoped_var = ScopedVar {
                        // TODO IMPORTANT surely this should be `self`???
                        // name: target_item.ident.clone(),
                        name: "self".to_string(),
                        // TODO how do we know if we have `foo(mut self)`?
                        mut_: is_mut,
                        type_: input_type.clone().into_rust_type2(global_data),
                    };
                    vars.push(scoped_var);
                } else {
                    // TODO what if input types contain type params that have been resolved? need a `fn update_type(possibly_resolved_generics, type)`
                    let scoped_var = ScopedVar {
                        name,
                        mut_: is_mut,
                        type_: input_type.into_rust_type2(global_data),
                    };
                    // dbg!(&scoped_var);
                    vars.push(scoped_var);
                }
            }
        }
        RustImplItemItemNoJs::Const => todo!(),
    }

    // Because item definitions can appear after they are used, we can't simply add them to the scope as they are handled. We instead need to go through all the stmts and record the defined items before we do a second pass to actually handle/parse the stmts.
    let scoped_item_defs = match &rust_impl_item.item {
        RustImplItemItemNoJs::Fn(_static_, fn_info) => {
            fn_info
                .stmts
                .iter()
                .filter_map(|stmt_ref| {
                    match stmt_ref {
                        StmtsRef::Item(item_ref) => {
                            match item_ref {
                                ItemRef::StructOrEnum(index) => Some(*index),
                                ItemRef::Fn(index) => Some(*index),
                                // TODO can consts be scoped or are they always global??
                                ItemRef::Const(index) => Some(*index),
                                ItemRef::Trait(index) => Some(*index),
                                ItemRef::Mod(_) => todo!(),
                                ItemRef::Use(_) => todo!(),
                                ItemRef::Macro => todo!(),
                                ItemRef::Impl(_) => None,
                            }
                        }
                        _ => None,
                    }
                })
                .collect()
        }
        RustImplItemItemNoJs::Const => todo!(),
    };

    // Create scope for impl method/fn body
    info!("handle_item_impl new scope");

    global_data.scopes.push(GlobalDataScope {
        variables: vars,
        _look_in_outer_scope: false,
        use_mappings: Vec::new(),
        items: scoped_item_defs,
    });

    // TODO this approach for bool_and and add_assign is very limited and won't be possible if 2 differnt types need 2 different implementations for the same method name
    // TODO need to look up whether path is eg `rust_std::RustBool`, not just the item name
    // TODO see commented out code below this fn for old eg RustInteger + add_assign mappings/updates
    let _n_stmts = impl_item_fn.block.stmts.len();

    let returns_non_mut_ref_val = match &impl_item_fn.sig.output {
        ReturnType::Default => false,
        ReturnType::Type(_, type_) => matches!(&**type_, Type::Reference(_)),
    };

    // so this is just used for the inital/one off analysis of the impl method, and when actually going through the code from main, we will store a self var in the scope like the other vars????
    // We are adding other input vars to the scope further up the code, why not just add self to those vars? By definition, if there is self we are dealing with a method on an *instance* of a struct/enum, so *if the instance item is generic* we need to either:
    // 1. generate a new method for for whatever the concrete type of the generic is for this particular instance (assuming it is known by this point)
    // 2. use the same method for the different generic concrete types, and make sure any interaction with a generic type is generalisable, ie use .eq() in place of ===, etc.
    // I think we want to use 2. in all cases  except where we have T::associated_fn() because then we need to replace this with the actual Foo::associated_fn() or whatever.
    // Ok but how do we get the type of self, so that we can use it in the fn, eg `self.some_field_with_type_we_want_to_know` or to return from the method... bearing in mind that (for types with generics) we won't know the type (well we'll know the type just not any resolved generics) until the method is called like `instance.the_method()`, so it seems like this is something we can just handle in `handle_expr_method_call`, since that is the point we will have:
    // 1. the most recent resolved Self type
    // 2. the args of the method to see if they can be used to resolved any generics
    // So I think for now we can just record RustType::InstanceSelf as the return type? It doesn't need a type because only the arguments can help narrow the type, and we will handle that in handle_expr_method_call
    //
    // Need to consider situation where we eg return a &self or &mut self to a var, then later interaction with that var determine some generics, in which case do we need to also update the generics on the original var?
    //
    // Given signatures like `pub fn map<U, F>(self, f: F) -> Option<U>`, What do we need to store in MemberInfo to be able to know that the return type is Option<U> where U is the return type of the closure argument?
    //
    // Remember method can return Foo<T> or just T, or Foo<U> (ie Some(5).map(...))
    //
    // NOTE there is a difference between returning self or &self or &mut self, and some other instance that also has type Self, but is not actually self
    //
    // What about `let foo: Foo<i32> = foo_maker.method(5)` or something?
    //

    let body_stmts = match &rust_impl_item.item {
        RustImplItemItemNoJs::Fn(_static, fn_info) => {
            //

            parse_fn_body_stmts(
                false,
                returns_non_mut_ref_val,
                true,
                &fn_info.stmts,
                global_data,
                current_module_path,
            )
        }
        RustImplItemItemNoJs::Const => todo!(),
    };

    let body_stmts = Some(body_stmts);

    global_data.scopes.pop();

    // TODO no idea why body_stmts is an `Option`
    // Push to rust_impl_items
    if let Some((body_stmts, _return_type)) = body_stmts {
        let is_pub = match impl_item_fn.vis {
            Visibility::Public(_) => true,
            Visibility::Restricted(_) => todo!(),
            Visibility::Inherited => false,
        };
        let static_ = impl_item_fn
            .sig
            .inputs
            .first()
            .map_or(true, |input| match input {
                FnArg::Receiver(_) => false,
                FnArg::Typed(_) => true,
            });

        let fn_info = match rust_impl_item.item.clone() {
            RustImplItemItemNoJs::Fn(_static, fn_info) => fn_info,
            RustImplItemItemNoJs::Const => todo!(),
        };

        let js_fn = JsFn {
            iife: false,
            public: is_pub,
            export: false,
            // TODO
            async_: false,
            is_method: true,
            name: Ident::Syn(impl_item_fn.sig.ident.clone()),
            input_names: js_input_names,
            body_stmts,
        };

        RustImplItemItemJs::Fn(static_, fn_info, js_fn)
    } else {
        todo!()
    }
}

pub fn handle_item_impl(
    // item_impl: &ItemImpl,
    index: usize,
    _at_module_top_level: bool,
    global_data: &mut GlobalData,
    current_module_path: &[String],
) -> Vec<JsStmt> {
    let rust_impl_block = match global_data.item_defs[index].clone() {
        ItemDef::Impl(impl_block) => impl_block,
        other => {
            dbg!(index);
            dbg!(other);
            todo!()
        }
    };
    let item_impl = rust_impl_block.syn.clone();

    let debug_self_type = match &*item_impl.self_ty {
        Type::Path(type_path) => format!("{:?}", type_path.path.segments),
        _ => format!("{:?}", item_impl.self_ty),
    };
    let span = debug_span!("handle_item_impl", debug_self_type = ?debug_self_type);
    let _guard = span.enter();

    let rust_impl_block_generics = rust_impl_block.generics;

    let trait_path_and_name = rust_impl_block.trait_;

    let target_rust_type = rust_impl_block.target.into_rust_type2(global_data);
    let is_target_type_param = matches!(target_rust_type, RustType2::TypeParam(_));

    global_data
        .impl_block_target_type
        .push(target_rust_type.clone());

    let rust_impl_items = rust_impl_block
        .rust_items
        .into_iter()
        .map(|item| RustImplItemJs {
            ident: item.ident.clone(),
            item: match &item.item {
                RustImplItemItemNoJs::Fn(_static_, fn_info) => {
                    // TODO IMPORTANT reuse code from handle_item_fn

                    // RustImplItemItemJs::Fn(static_, fn_info, js)
                    handle_impl_item_fn(
                        // impl_item,
                        match &fn_info.syn {
                            FnInfoSyn::Standalone(_) => todo!(),
                            FnInfoSyn::Impl(impl_item_fn) => impl_item_fn,
                            FnInfoSyn::Trait(_) => todo!(),
                        },
                        global_data,
                        current_module_path,
                        &target_rust_type,
                        &item,
                    )
                }
                RustImplItemItemNoJs::Const => RustImplItemItemJs::Const(todo!()),
            },
        })
        .collect::<Vec<_>>();

    let js_impl_block = JsImplBlock2 {
        _index: index,
        _generics: rust_impl_block_generics,
        trait_: trait_path_and_name,
        target: target_rust_type.clone(),
        items: rust_impl_items
            .into_iter()
            .map(|x| (false, x))
            .collect::<Vec<_>>(),
    };
    global_data.impl_block_target_type.pop();

    global_data.impl_blocks.push(js_impl_block.clone());

    let class_stmt = if is_target_type_param || matches!(target_rust_type, RustType2::I32) {
        let static_fields = js_impl_block
            .items
            .iter()
            .cloned()
            .filter_map(|(_used, item)| match item.item {
                RustImplItemItemJs::Fn(_, _, _) => None,
                RustImplItemItemJs::Const(js_local) => Some(js_local),
            })
            .collect::<Vec<_>>();
        let methods = js_impl_block
            .items
            .iter()
            .cloned()
            .filter_map(|(_used, item)| match item.item {
                RustImplItemItemJs::Fn(static_, _fn_info, js_fn) => {
                    Some((js_impl_block.js_name(), static_, js_fn))
                }
                RustImplItemItemJs::Const(_) => None,
            })
            .collect::<Vec<_>>();

        JsStmt::Class(JsClass {
            public: false,
            export: false,
            tuple_struct: false,
            name: js_impl_block.js_name(),
            inputs: Vec::new(),
            static_fields,
            methods,
            // TODO this is good evidence why we shouldn't be storing Rust stuff in a JS type
            rust_name: "implblockdonotuse".to_string(),
            module_path: vec!["implblockdonotuse".to_string()],
            scope_id: None,
            is_impl_block: true,
        })
    } else {
        JsStmt::Expr(JsExpr::Vanish, false)
    };
    let mut stmts = vec![class_stmt];

    fn prelude_item_def_name_to_js(item_def_name: &str) -> &'static str {
        match item_def_name {
            "i32" => "Number",
            "String" => "String",
            "str" => "String",
            // "bool" => "Boolean",
            // TODO why Bool not bool?????
            "Bool" => "Boolean",
            "Box" => "donotuse",
            "Option" => "donotuse",
            "Vec" => "Array",
            _ => todo!(),
        }
    }

    let prelude_module = global_data.get_module(&[PRELUDE_MODULE_PATH.to_string()]);
    let mut dedup_rust_prelude_definitions = prelude_module
        .items
        .iter()
        .filter_map(|item_ref| match item_ref {
            ItemRef::StructOrEnum(index) => {
                let item = &global_data.item_defs[*index];
                let item_def = match item {
                    ItemDef::StructEnum(item_def) => item_def,
                    _ => todo!(),
                };
                let new_name = prelude_item_def_name_to_js(&item_def.ident);
                if new_name != "donotuse" {
                    Some((new_name, item_def))
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    // Need to use sort by because of lifetimes: https://users.rust-lang.org/t/sort-by-key-and-probably-a-simple-lifetime-issue/73358
    dedup_rust_prelude_definitions
        .sort_by(|(js_name, _item_def), (js_name2, _item_def2)| js_name.cmp(js_name2));
    dedup_rust_prelude_definitions
        .dedup_by(|(js_name, _item_def), (js_name2, _item_def2)| js_name == js_name2);

    // Push stmts like `Number.prototype.foo = bar.prototype.foo`
    for (js_name, prelude_item_def) in &dedup_rust_prelude_definitions {
        // If impl block matches the prelude type, push prototype assignments for each item in block
        if prelude_item_def.impl_block_ids.contains(&index) {
            for (_is_used, item) in &js_impl_block.items {
                // TODO only add if `is_used == true`
                let item_name = Ident::String(item.ident.clone());
                let block_name = &js_impl_block.js_name();

                // methods on primative values like i32 need to be added to eg `RustInteger` rather than `Number` if they take `&mut self`
                let method_self_mut_ref = match &item.item {
                    RustImplItemItemJs::Fn(_, fn_info, _) => fn_info
                        .inputs_types
                        .first()
                        .is_some_and(|(is_self, _is_mut, _name, type_)| {
                            *is_self
                                && matches!(
                                    // TODO this is unnecessary, to I like not having RustType imported, so add a pub method to RustType instead
                                    type_.clone().into_rust_type2(global_data),
                                    RustType2::MutRef(_)
                                )
                        }),
                    RustImplItemItemJs::Const(_) => todo!(),
                };
                let js_name = if *js_name == "Number" && method_self_mut_ref {
                    "RustInteger"
                } else {
                    js_name
                };

                stmts.push(JsStmt::Raw(format!(
                    "{js_name}.prototype.{item_name} = {block_name}.prototype.{item_name}"
                )))
            }
        }
    }

    stmts
}

pub fn handle_item_struct(
    index: usize,
    _at_module_top_level: bool,
    global_data: &mut GlobalData,
    current_module_path: &[String],
) -> JsStmt {
    let item = &global_data.item_defs[index];
    let item_def = match item {
        ItemDef::StructEnum(actual) => match &actual.struct_or_enum_info {
            StructOrEnumDefitionInfo2::Struct(_struct_def) => actual.clone(),
            StructOrEnumDefitionInfo2::Enum(_enum_def) => {
                todo!()
            }
        },
        _ => todo!(),
    };

    let item_struct = match &item_def.struct_or_enum_info {
        StructOrEnumDefitionInfo2::Struct(struct_def_info) => &struct_def_info.syn_object,
        StructOrEnumDefitionInfo2::Enum(_) => todo!(),
    };
    let name = item_struct.ident.to_string();
    // dbg!(&global_data.scopes);
    debug!(name = ?name, "handle_item_struct");
    // Attribute {
    //     pound_token: Pound,
    //     style: AttrStyle::Outer,
    //     bracket_token: Bracket,
    //     meta: Meta::List {
    //         path: Path {
    //             leading_colon: None,
    //             segments: [
    //                 PathSegment {
    //                     ident: Ident {
    //                         sym: derive,
    //                         span: bytes(154..160),
    //                     },
    //                     arguments: PathArguments::None,
    //                 },
    //             ],
    //         },
    //         delimiter: MacroDelimiter::Paren(
    //             Paren,
    //         ),
    //         tokens: TokenStream [
    //             Ident {
    //                 sym: PartialEq,
    //                 span: bytes(161..170),
    //             },
    //         ],
    //     },
    // },

    let is_copy = item_struct.attrs.iter().any(|attr| match &attr.meta {
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

    // Populate methods and fields
    // TODO to avoid mut and immut ref clashing - fix this
    let mut methods = Vec::new();
    let static_fields = Vec::new();

    if is_copy {
        let stmt = JsStmt::Raw("return JSON.parse(JSON.stringify(this));".to_string());
        methods.push((
            Ident::String(name.clone()),
            false,
            JsFn {
                iife: false,
                public: false,
                export: false,
                async_: false,
                is_method: true,
                name: Ident::Str("copy"),
                input_names: Vec::new(),
                body_stmts: vec![stmt],
            },
        ));
    }

    // TODO deriving PartialEq for our Option causes a clash with the proper Option, so just manually add it for now
    // fn eq(&self, other: &Self) -> bool
    if name == "Option" {
        let stmt = JsStmt::Raw("return JSON.stringify(this) === JSON.stringify(other)".to_string());
        methods.push((
            Ident::String(name.clone()),
            false,
            JsFn {
                iife: false,
                public: false,
                export: false,
                async_: false,
                is_method: true,
                name: Ident::Str("eq"),
                input_names: vec![Ident::Str("other")],
                body_stmts: vec![stmt],
            },
        ));
    }

    let (tuple_struct, inputs) = match &item_struct.fields {
        Fields::Named(fields_named) => (
            false,
            fields_named
                .named
                .iter()
                .map(|field| Ident::Syn(field.ident.as_ref().unwrap().clone()))
                .collect::<Vec<_>>(),
        ),
        Fields::Unnamed(fields_unnamed) => (
            true,
            fields_unnamed
                .unnamed
                .iter()
                .enumerate()
                // TODO create Ident Variant for suffixing i to avoid allocating
                .map(|(i, _field)| Ident::String(format!("arg{i}")))
                .collect::<Vec<_>>(),
        ),
        Fields::Unit => todo!(),
    };

    let mut js_class = JsClass {
        export: false,
        public: match item_struct.vis {
            Visibility::Public(_) => true,
            Visibility::Restricted(_) => todo!(),
            Visibility::Inherited => false,
        },
        name: item_def.js_name.clone(),
        tuple_struct,
        inputs,
        static_fields,
        methods,
        rust_name: item_struct.ident.to_string(),
        is_impl_block: false,
        module_path: current_module_path.to_vec(),
        // scope_id: global_data.scope_id_as_option(),
        scope_id: None,
    };

    let mut dedup_impl_block_ids = item_def.impl_block_ids.clone();
    dedup_impl_block_ids.sort();
    dedup_impl_block_ids.dedup();
    for impl_block_id in &dedup_impl_block_ids {
        // let js_impl_block = &global_data.impl_blocks[*impl_block_id];

        // TODO IMPORTANT we are parsing impl block methods below when it is also being done in handle_item_impl(), this probably breaks assumptions eg mutates data twice etc

        let rust_impl_block = match global_data.item_defs[*impl_block_id].clone() {
            ItemDef::Impl(impl_block) => impl_block,
            other => {
                dbg!(index);
                dbg!(other);
                todo!()
            }
        };

        let target_rust_type = rust_impl_block.target.into_rust_type2(global_data);

        global_data
            .impl_block_target_type
            .push(target_rust_type.clone());

        let rust_impl_items = rust_impl_block
            .rust_items
            .into_iter()
            .map(|item| RustImplItemJs {
                ident: item.ident.clone(),
                item: match &item.item {
                    RustImplItemItemNoJs::Fn(_static_, fn_info) => {
                        // TODO IMPORTANT reuse code from handle_item_fn

                        // RustImplItemItemJs::Fn(static_, fn_info, js)
                        handle_impl_item_fn(
                            // impl_item,
                            match &fn_info.syn {
                                FnInfoSyn::Standalone(_) => todo!(),
                                FnInfoSyn::Impl(impl_item_fn) => impl_item_fn,
                                FnInfoSyn::Trait(_) => todo!(),
                            },
                            global_data,
                            current_module_path,
                            &target_rust_type,
                            &item,
                        )
                    }
                    RustImplItemItemNoJs::Const => RustImplItemItemJs::Const(todo!()),
                },
            })
            .collect::<Vec<_>>();

        let js_impl_block = JsImplBlock2 {
            _index: index,
            _generics: rust_impl_block.generics,
            trait_: rust_impl_block.trait_,
            target: target_rust_type.clone(),
            items: rust_impl_items
                .into_iter()
                .map(|x| (false, x))
                .collect::<Vec<_>>(),
        };

        let is_generic_impl = matches!(js_impl_block.target, RustType2::TypeParam(_));

        for (_used, impl_item) in &js_impl_block.items {
            // TODO implement used
            // TODO What about `impl Foo for T {}`? This means we need to add prototype fields, not methods?
            match &impl_item.item {
                RustImplItemItemJs::Fn(static_, _fn_info, js_fn) => {
                    if is_generic_impl {
                        js_class.static_fields.push(JsLocal {
                            public: false,
                            export: false,
                            type_: LocalType::None,
                            lhs: LocalName::Single(js_fn.name.clone()),
                            value: JsExpr::Path(PathIdent::Path(
                                [
                                    js_impl_block.js_name(),
                                    Ident::Str("prototype"),
                                    js_fn.name.clone(),
                                ]
                                .to_vec(),
                            )),
                        });
                    } else {
                        js_class.methods.push((
                            Ident::String(item_def.ident.clone()),
                            *static_,
                            js_fn.clone(),
                        ));
                    }
                }
                RustImplItemItemJs::Const(_) => todo!(),
            }
        }
    }

    JsStmt::Class(js_class)
}

pub fn _handle_item_mod(
    item_mod: ItemMod,
    global_data: &mut GlobalData,
    current_module_path: &mut Vec<String>,
) {
    let span = debug_span!("handle_item_mod", current_module_path = ?current_module_path);
    let _guard = span.enter();

    // Notes
    // The `self` keyword is only allowed as the first segment of a path
    // The `crate` keyword is only allowed as the first segment of a path
    // The `super` keyword is only allowed as *one* of the first segments of a path, before any named modules
    // The `super` keyword can be used in multiple segments of a path
    // self might not be that important but crate is and has similar requirements
    // modules *cannot* access anything in their parent scope without explicitly using crate or super, therefore nesting the modules in JS is of no benefit
    // Also need to consider how to use the same Rust module/JS function in multiple places - even though modules are just items and therefore immutable, we still can't have the duplication of code because this could be huge in certain cases. So all modules, both crate modules and sub modules, need to be defined at the top level - no they just need to be accessible from the top level using crate and super, nesting modules doesn't mean duplication because they will always be access at that path anyway.
    // We *could* use a solution requiring replacing self:: paths with absolute paths since self:: *always refers to a module path and self in a method always uses self. since self is an instance not a type/path

    current_module_path.push(item_mod.ident.to_string());
    let mut module_path_copy = current_module_path.clone();
    // TODO get rid of this
    if let Some(first) = module_path_copy.first() {
        if first == "crate" {
            module_path_copy.remove(0);
        }
    }

    let _items = if let Some(content) = &item_mod.content {
        // TODO how does `mod bar { mod foo; }` work?
        content.1.clone()
    } else if let Some(crate_path) = &global_data._crate_path {
        let mut file_path = crate_path.clone();
        file_path.push("src");
        if module_path_copy.is_empty() {
            file_path.push("main.rs");
        } else {
            let last = module_path_copy.last_mut().unwrap();
            last.push_str(".rs");
            file_path.extend(module_path_copy);
        }
        let code = fs::read_to_string(&file_path).unwrap();
        syn::parse_file(&code).unwrap().items
    } else {
        panic!("not allowed `mod foo` outside of crate")
    };

    // NOTE excluding use of attributes, only modules that are the directory parent can `mod foo`, any anywhere else we have to use `use` not `mod`.
    // In rust `mod foo` is largely redundant except for defining visibility and attributes https://stackoverflow.com/questions/32814653/why-is-there-a-mod-keyword-in-rust

    let current_module_path_copy = current_module_path.clone();
    let _js_stmt_submodule = JsModule {
        public: match item_mod.vis {
            Visibility::Public(_) => true,
            Visibility::Restricted(_) => todo!(),
            Visibility::Inherited => false,
        },
        name: Ident::Syn(item_mod.ident),
        module_path: current_module_path_copy.clone(),
        stmts: Vec::new(),
    };
    // convert from `syn` to `JsStmts`, passing the updated `current_file_path` to be used by any `mod` calls within the new module

    // global_data.transpiled_modules.push(js_stmt_submodule);
    // let stmts = js_stmts_from_syn_items(current_module_path, global_data);
    // let js_stmt_module = global_data
    //     .transpiled_modules
    //     .iter_mut()
    //     .find(|tm| tm.module_path == current_module_path_copy)
    //     .unwrap();
    // js_stmt_module.stmts = stmts;
    current_module_path.pop();

    // TODO shouldn't be using .export field as this is for importing from separate files. We don't want to add "export " to public values in a module, simply add them to the return statement of the function.
}

pub fn handle_item_trait(
    index: usize,
    _at_module_top_level: bool,
    global_data: &mut GlobalData,
    _current_module_path: &[String],
) -> JsStmt {
    debug!("handle_item_trait");

    let trait_def = match &global_data.item_defs[index] {
        ItemDef::Trait(trait_def) => trait_def,
        _ => todo!(),
    };

    if !trait_def.default_impls.is_empty() {
        let _methods = trait_def
            .default_impls
            .iter()
            .map(|fn_info| {
                let _js_fn = JsFn {
                    iife: false,
                    public: false,
                    export: false,
                    async_: false,
                    is_method: true,
                    name: fn_info.js_name.clone(),
                    input_names: todo!(),
                    body_stmts: todo!(),
                };
            })
            .collect::<Vec<_>>();

        JsStmt::Class(JsClass {
            public: todo!(),
            export: todo!(),
            tuple_struct: todo!(),
            name: todo!(),
            inputs: todo!(),
            static_fields: todo!(),
            methods: todo!(),
            rust_name: todo!(),
            module_path: todo!(),
            scope_id: todo!(),
            is_impl_block: todo!(),
        })
    } else {
        JsStmt::Raw("".to_string())
    }

    // IMPORTANT TODO I think we need to be adding scoped traits to .scopes here but we are not
}
