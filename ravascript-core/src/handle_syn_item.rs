use biome_formatter::{FormatLanguage, IndentStyle, IndentWidth};
use biome_js_formatter::{context::JsFormatOptions, JsFormatLanguage};
use biome_js_parser::JsParserOptions;
use biome_js_syntax::JsFileSource;
use heck::{AsKebabCase, AsLowerCamelCase, AsPascalCase};
use quote::quote;
use std::{
    default,
    fmt::{self, Debug},
    fs,
    net::ToSocketAddrs,
    path::{Path, PathBuf},
};
use syn::{
    parenthesized, parse_macro_input, BinOp, DeriveInput, Expr, ExprAssign, ExprBlock, ExprCall,
    ExprClosure, ExprMatch, ExprMethodCall, ExprPath, Fields, FnArg, GenericArgument, GenericParam,
    Ident, ImplItem, ImplItemFn, Item, ItemConst, ItemEnum, ItemFn, ItemImpl, ItemMod, ItemStruct,
    ItemTrait, ItemUse, Lit, Local, Macro, Member, Meta, Pat, PathArguments, PathSegment,
    ReturnType, Stmt, TraitItem, Type, TypeParamBound, UnOp, UseTree, Visibility, WherePredicate,
};
use tracing::{debug, debug_span, info, span, warn};

use crate::{
    camel, get_item_impl_unique_id,
    handle_syn_expr::handle_expr,
    handle_syn_stmt::handle_stmt,
    js_ast::{JsClass, JsExpr, JsFn, JsLocal, JsModule, JsStmt, LocalName, LocalType},
    js_stmts_from_syn_items, parse_fn_body_stmts, parse_fn_input_or_field, ConstDef,
    EnumDefinitionInfo, EnumVariantInfo, EnumVariantInputsInfo, FnInfo, GlobalData,
    GlobalDataScope, ItemDefinition, ItemDefintionImpls, JsImplBlock2, JsImplItem, RustGeneric,
    RustImplItemItemJs, RustImplItemItemNoJs, RustImplItemJs, RustImplItemNoJs,
    RustTraitDefinition, RustType, RustTypeParam, RustTypeParamValue, ScopedVar,
    StructDefinitionInfo, StructFieldInfo, StructOrEnumDefitionInfo, PRELUDE_MODULE_PATH,
};

pub fn handle_item_fn(
    item_fn: &ItemFn,
    // For keeping track of whether we are parsing items at the module level or in a fn scope, so that we know whether we need to add the items to `.scopes` or not.
    // Good also keep track using a field on global data, but for now seems less error prone to pass values to handle fns because it is always clear whether we are at the top level based on whether the item is being parsed within `handle_statments()`
    at_module_top_level: bool,
    global_data: &mut GlobalData,
    current_module: &[String],
) -> JsStmt {
    let name = item_fn.sig.ident.to_string();
    let span = debug_span!("handle_item_fn", name = ?name);
    let _guard = span.enter();

    let ignore = if let Some(thing) = item_fn.attrs.first() {
        match &thing.meta {
            Meta::Path(path) => {
                if let Some(seg) = path.segments.first() {
                    seg.ident.to_string() == "ignore".to_string()
                } else {
                    false
                }
            }
            _ => false,
        }
    } else {
        false
    };

    let js_name = if !at_module_top_level {
        camel(name.clone())
    } else {
        let in_module_level_duplicates = global_data
            .duplicates
            .iter()
            .find(|dup| dup.name == name && &dup.original_module_path == current_module);

        if let Some(dup) = in_module_level_duplicates {
            dup.namespace
                .iter()
                .map(|seg| camel(seg))
                .collect::<Vec<_>>()
                .join("__")
        } else {
            camel(name.clone())
        }
    };

    // let type_params = item_fn
    //     .sig
    //     .generics
    //     .params
    //     .iter()
    //     .map(|generic_param| match generic_param {
    //         GenericParam::Lifetime(_) => todo!(),
    //         GenericParam::Type(type_param) => type_param.ident.to_string(),
    //         GenericParam::Const(_) => todo!(),
    //     })
    //     .collect::<Vec<_>>();

    // let rust_type_params = type_params
    //     .iter()
    //     .map(|tp_name| RustTypeParam {
    //         name: tp_name.clone(),
    //         type_: RustTypeParamValue::Unresolved,
    //     })
    //     .collect::<Vec<_>>();

    // let inputs = &item_fn
    //     .sig
    //     .inputs
    //     .iter()
    //     .filter_map(|input| match input {
    //         FnArg::Receiver(_) => None,
    //         FnArg::Typed(pat_type) => Some(match &*pat_type.pat {
    //             Pat::Ident(pat_ident) => parse_fn_input_or_field(
    //                 &*pat_type.ty,
    //                 pat_ident.mutability.is_some(),
    //                 &Vec::new(),
    //                 current_module,
    //                 global_data,
    //             ),
    //             _ => {
    //                 todo!();
    //             }
    //         }),
    //     })
    //     .collect::<Vec<_>>();

    // let fn_info = FnInfo {
    //     ident: name,
    //     generics: type_params,
    //     inputs_types: inputs.clone(),
    //     return_type: match &item_fn.sig.output {
    //         ReturnType::Default => RustType::Unit,
    //         ReturnType::Type(_, type_) => {
    //             // TODO Note we are passing false for has_mut_keyword because it doesn't apply to fn returns
    //             parse_fn_input_or_field(
    //                 type_,
    //                 false,
    //                 &rust_type_params,
    //                 current_module,
    //                 global_data,
    //             )
    //         }
    //     },
    // };

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

    // Create new scope for fn vars
    global_data.push_new_scope(false, Vec::new());

    // record which vars are mut and/or &mut
    let mut copy_stmts = Vec::new();
    for input in &item_fn.sig.inputs {
        match input {
            FnArg::Receiver(_) => {}
            FnArg::Typed(pat_type) => match &*pat_type.pat {
                Pat::Ident(pat_ident) => {
                    // let mut_ref = match &*pat_type.ty {
                    //     Type::Reference(type_reference) => type_reference.mutability.is_some(),
                    //     _ => false,
                    // };

                    // if pat_ident.mutability.is_some() || mut_ref {
                    //     global_data.scopes.last_mut().unwrap().0.push(ScopedVar {
                    //         name: pat_ident.ident.to_string(),
                    //         mut_: pat_ident.mutability.is_some(),
                    //         mut_ref,
                    //         type_: "".to_string(),
                    //     })
                    // }
                    let mut scoped_var = ScopedVar {
                        name: pat_ident.ident.to_string(),
                        mut_: pat_ident.mutability.is_some(),
                        type_: RustType::Todo,
                    };
                    let input_type = parse_fn_input_or_field(
                        &*pat_type.ty,
                        pat_ident.mutability.is_some(),
                        &Vec::new(),
                        current_module,
                        global_data,
                    );
                    match &input_type {
                        // TypeOrVar::RustType(rust_type) => scoped_var.type_ = rust_type,
                        // TypeOrVar::Var(found_var) => {
                        //     scoped_var.mut_ref = scoped_var.mut_ref || found_var.mut_ref;
                        //     scoped_var.type_ = found_var.type_;
                        // }
                        // TypeOrVar::Unknown => {}

                        // FnReturnType::RustType(mut_ref, rust_type) => {
                        //     scoped_var.mut_ref = *mut_ref;
                        //     scoped_var.type_ = rust_type.clone();
                        // }
                        // FnReturnType::Unknown => {}
                        RustType::NotAllowed => todo!(),
                        RustType::Unknown => {}
                        RustType::Todo => todo!(),
                        RustType::Unit => {
                            scoped_var.type_ = input_type.clone();
                        }
                        RustType::Never => {
                            scoped_var.type_ = input_type.clone();
                        }
                        RustType::I32 => {
                            scoped_var.type_ = input_type.clone();
                        }
                        RustType::F32 => {
                            scoped_var.type_ = input_type.clone();
                        }
                        RustType::Bool => {
                            scoped_var.type_ = input_type.clone();
                        }
                        RustType::String => {
                            scoped_var.type_ = input_type.clone();
                        }
                        RustType::StructOrEnum(_, _, _, _) => {
                            scoped_var.type_ = input_type.clone();
                        }
                        // RustType::Enum(_,_,_) => {
                        //     scoped_var.type_ = input_type.clone();
                        // }
                        RustType::Vec(_) => {
                            scoped_var.type_ = input_type.clone();
                        }
                        RustType::Array(_) => {
                            scoped_var.type_ = input_type.clone();
                        }
                        RustType::Tuple(_) => {
                            scoped_var.type_ = input_type.clone();
                        }
                        RustType::MutRef(rust_type) => {
                            scoped_var.type_ = RustType::MutRef(rust_type.clone());
                        }
                        RustType::Fn(_, _, _, _, _) => todo!(),
                        RustType::Option(_) => todo!(),
                        RustType::Result(_) => todo!(),
                        RustType::TypeParam(_) => todo!(),
                        RustType::ImplTrait(_) => todo!(),
                        RustType::ParentItem => todo!(),
                        RustType::UserType(_, _) => todo!(),
                        RustType::Ref(_) => todo!(),
                        RustType::Closure(_, _) => todo!(),
                        RustType::FnVanish => todo!(),
                        RustType::Box(_) => todo!(),
                    }
                    // record add var to scope
                    global_data
                        .scopes
                        .last_mut()
                        .unwrap()
                        .variables
                        .push(scoped_var);

                    // a mut input of a copy type like `mut num: i32` must be converted to `RustInteger`
                    // TODO need to add this to `handle_impl_fn_item()`
                    if pat_ident.mutability.is_some() {
                        copy_stmts.push(JsStmt::Local(JsLocal {
                            public: false,
                            export: false,
                            type_: LocalType::None,
                            lhs: LocalName::Single(pat_ident.ident.to_string()),
                            // value: JsExpr::MethodCall(
                            //     Box::new(JsExpr::Path(vec![pat_ident.ident.to_string()])),
                            //     "copy".to_string(),
                            //     Vec::new(),
                            // ),
                            value: match &input_type {
                                RustType::Todo => todo!(),
                                RustType::Unit => todo!(),
                                RustType::I32 => {
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
                                        vec!["RustInteger".to_string()],
                                        vec![JsExpr::Path(vec![pat_ident.ident.to_string()])],
                                    )
                                }
                                RustType::F32 => todo!(),
                                RustType::Bool => todo!(),
                                RustType::String => todo!(),
                                RustType::StructOrEnum(_, _, _, _) => todo!(),
                                // RustType::Enum(_,_,_) => todo!(),
                                RustType::NotAllowed => todo!(),
                                RustType::Unknown => todo!(),
                                RustType::Never => todo!(),
                                RustType::Vec(_) => todo!(),
                                RustType::Array(_) => todo!(),
                                RustType::Tuple(_) => todo!(),
                                RustType::MutRef(_) => todo!(),
                                RustType::Fn(_, _, _, _, _) => todo!(),
                                RustType::Option(_) => todo!(),
                                RustType::Result(_) => todo!(),
                                RustType::TypeParam(_) => todo!(),
                                RustType::ImplTrait(_) => todo!(),
                                RustType::ParentItem => todo!(),
                                RustType::UserType(_, _) => todo!(),
                                RustType::Ref(_) => todo!(),
                                RustType::Closure(_, _) => todo!(),
                                RustType::FnVanish => todo!(),
                                RustType::Box(_) => todo!(),
                            },
                        }))
                    }
                }
                _ => {}
            },
        }
    }

    let stmt = if ignore {
        JsStmt::Expr(JsExpr::Vanish, false)
    } else {
        // If we are returning a type which is *not* &mut, then we need to `.copy()` or `.inner()` if the value being returned is mut (if the value is &mut, the compiler will have ensured there is a deref, so we will have already added a `.copy()` or `.inner()`).
        let returns_non_mut_ref_val = match &item_fn.sig.output {
            ReturnType::Default => false,
            ReturnType::Type(_, type_) => match &**type_ {
                Type::Reference(_) => false,
                _ => true,
            },
        };
        // dbg!(&item_fn.block.stmts);

        let (body_stmts, return_type) = parse_fn_body_stmts(
            false,
            returns_non_mut_ref_val,
            true,
            &item_fn.block.stmts,
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
            name: js_name,
            input_names: item_fn
                .sig
                .inputs
                .iter()
                .map(|input| match input {
                    FnArg::Receiver(_) => todo!(),
                    FnArg::Typed(pat_type) => match &*pat_type.pat {
                        Pat::Ident(pat_ident) => camel(&pat_ident.ident),
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
    global_data.pop_scope();

    stmt
}

pub fn handle_item_const(
    item_const: &ItemConst,
    at_module_top_level: bool,
    global_data: &mut GlobalData,
    current_module: &[String],
) -> JsStmt {
    let mut name = item_const.ident.to_string();
    debug!(name = ?name, "handle_item_const");

    // NOTE we only push scoped definitions because module level definition are already pushed in extract_data_populate_item_definitions
    // if !at_module_top_level {
    //     let generics = item_const
    //         .generics
    //         .params
    //         .iter()
    //         .map(|p| match p {
    //             GenericParam::Lifetime(_) => todo!(),
    //             GenericParam::Type(type_param) => type_param.ident.to_string(),
    //             GenericParam::Const(_) => todo!(),
    //         })
    //         .collect::<Vec<_>>();

    //     let generics_type_params = generics
    //         .iter()
    //         .map(|name| RustTypeParam {
    //             name: name.clone(),
    //             type_: RustTypeParamValue::Unresolved,
    //         })
    //         .collect::<Vec<_>>();

    //     let rust_type = parse_fn_input_or_field(
    //         &item_const.ty,
    //         // TODO note mut isn't allowed for const so has_mut_keyword is false
    //         false,
    //         &generics_type_params,
    //         current_module,
    //         global_data,
    //     );

    //     let global_data_scope = global_data.scopes.last_mut().unwrap();
    //     global_data_scope.consts.push(ConstDef {
    //         name: name.clone(),
    //         type_: rust_type,
    //         syn_object: item_const.clone(),
    //     });
    // }

    // What is this doing?
    if let Some(dup) = global_data
        .duplicates
        .iter()
        .find(|dup| dup.name == name && dup.original_module_path == *current_module)
    {
        name = dup
            .namespace
            .iter()
            .map(|seg| camel(seg))
            .collect::<Vec<_>>()
            .join("__");
    }
    JsStmt::Local(JsLocal {
        export: false,
        public: match item_const.vis {
            Visibility::Public(_) => true,
            Visibility::Restricted(_) => todo!(),
            Visibility::Inherited => false,
        },
        type_: LocalType::Const,
        lhs: LocalName::Single(name),
        value: handle_expr(&*item_const.expr, global_data, current_module).0,
    })
}

/// We convert enum variants like Foo::Bar to Foo.bar because otherwise when the variant has arguments, syn is not able to distinguish it from an associated method, so we cannot deduce when Pascal or Camel case should be used, so stick to Pascal for both case.
/// We must store separate <variant name>Id fields because otherwise we end up in a situation where a variable containing an enum variant only contains the data returned the the method with that name and then we can't do myVariantVar === MyEnum::Variant because the lhs is data and the rhs is a function.
pub fn handle_item_enum(
    item_enum: ItemEnum,
    at_module_top_level: bool,
    global_data: &mut GlobalData,
    current_module: &[String],
) -> JsStmt {
    let enum_name = item_enum.ident.to_string();
    debug!(enum_name = ?enum_name, "handle_item_enum");
    // dbg!(item_enum.attrs);

    // Keep track of structs/enums in scope so we can subsequently add impl'd methods and then look up their return types when the method is called

    // TODO we can't just get the type of a variant from the enum definiton if it is generic, as the generic type info will only be available in the actual code where that particular instance of the enum is instantiated, or later on. Well actually we will be looking this up from where the enum is instantiated like `MyEnum::MyVariant` so maybe just return the type if there is no enum, else return something to flag that the generic type should be looked for??
    let generics = item_enum
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

    // TODO this is self referential... how to handle this?
    // We *shouldn't* be storing *resolved* generics (only unresolved generic names) in the list of struct/enum *items*. We *should* be storing resolved generics in the copies of the struct/enum we return from expressions, and possibly store as vars.
    // For the latter, where we want to store the return type of each "member" ie fields and methods, we can either:
    // Go through the members and resolve/update any generics used each time we get generic information (ie method input of item instantiation)
    // Not store the members and just look up methods etc on the syn object/value each time we want to eg check if a path is referrring to an associated fn??? (NOTE the main reason we started storing members in the first place is to store info about members defined impl blocks, but again could just store the syn object) I think just storing the syn objects initially is a good idea until I am clear what info is actually needed.
    // But all we are going to be doing with the syn objects is getting the return type and if there are generics, look to see if any of them have been resolved in `.generics` and we could do the same thing for `MemberInfo`?

    let members_for_scope = item_enum
        .variants
        .iter()
        .map(|v| {
            let inputs = v
                .fields
                .iter()
                .map(|f| {
                    let input_type = parse_fn_input_or_field(
                        &f.ty,
                        // NOTE it is not possible to make variant args mut in the definition
                        false,
                        &generics,
                        current_module,
                        global_data,
                    );
                    match &f.ident {
                        Some(input_name) => EnumVariantInputsInfo::Named {
                            ident: input_name.to_string(),
                            input_type,
                        },
                        None => EnumVariantInputsInfo::Unnamed(input_type),
                    }
                })
                .collect::<Vec<_>>();
            EnumVariantInfo {
                ident: v.ident.to_string(),
                inputs,
            }
        })
        .collect::<Vec<_>>();

    // let item_def = ItemDefinition {
    //     ident: item_enum.ident.to_string(),
    //     is_copy: item_enum.attrs.iter().any(|attr| match &attr.meta {
    //         Meta::Path(_) => todo!(),
    //         Meta::List(meta_list) => {
    //             let segs = &meta_list.path.segments;
    //             if segs.len() == 1 && segs.first().unwrap().ident == "derive" {
    //                 let tokens = format!("({})", meta_list.tokens);
    //                 let trait_tuple = syn::parse_str::<syn::TypeTuple>(&tokens).unwrap();
    //                 trait_tuple.elems.iter().any(|elem| match elem {
    //                     Type::Path(type_path) => {
    //                         let segs = &type_path.path.segments;
    //                         // TODO `Copy` could have been shadowed to need to do a proper lookup for trait with name `Copy` to check whether it is std::Copy or not.
    //                         segs.len() == 1 && segs.first().unwrap().ident == "Copy"
    //                     }
    //                     _ => todo!(),
    //                 })
    //             } else {
    //                 false
    //             }
    //         }
    //         Meta::NameValue(_) => todo!(),
    //     }),
    //     generics: item_enum
    //         .generics
    //         .params
    //         .iter()
    //         .map(|p| match p {
    //             GenericParam::Lifetime(_) => todo!(),
    //             GenericParam::Type(type_param) => type_param.ident.to_string(),
    //             GenericParam::Const(_) => todo!(),
    //         })
    //         .collect::<Vec<_>>(),
    //     struct_or_enum_info: StructOrEnumDefitionInfo::Enum(EnumDefinitionInfo {
    //         members: members_for_scope,
    //         syn_object: item_enum.clone(),
    //     }),
    //     impl_blocks: Vec::new(),
    // };

    // NOTE we only push scoped definitions because module level definition are already pushed in extract_data_populate_item_definitions
    // if !at_module_top_level {
    //     let global_data_scope = global_data.scopes.last_mut().unwrap();
    //     global_data_scope.item_definitons.push(item_def.clone());
    // }

    let class_name = item_enum.ident.to_string();

    // Populate methods and fields

    let mut static_fields = Vec::new();
    for variant in &item_enum.variants {
        static_fields.push(JsLocal {
            public: false,
            export: false,
            type_: LocalType::Static,
            lhs: LocalName::Single(format!("{}Id", camel(&variant.ident))),
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
                    lhs: LocalName::Single(format!(
                        "{}",
                        AsPascalCase(variant.ident.to_string()).to_string()
                    )),
                    value: JsExpr::New(
                        vec![class_name.clone()],
                        vec![JsExpr::LitStr(variant.ident.to_string()), JsExpr::Null],
                    ),
                    // Box::new(JsExpr::LitStr(variant.ident.to_string())),
                });
            }
        };
    }

    let mut class_name = item_enum.ident.to_string();

    let mut methods = Vec::new();
    let body_stmts = vec![
        JsStmt::Raw("this.id = id;".to_string()),
        JsStmt::Raw("this.data = data;".to_string()),
    ];
    // methods.push((
    //     item_enum.ident.to_string(),
    //     false,
    //     false,
    //     JsFn {
    //         iife: false,
    //         public: false,
    //         export: false,
    //         async_: false,
    //         is_method: true,
    //         name: "constructor".to_string(),
    //         input_names: vec!["id".to_string(), "data".to_string()],
    //         body_stmts,
    //     },
    // ));

    // TODO using syn types like JsImplItem {impl_item: ImplItem, body_stmts: Vec<JsStmt>} is no good if you want to manually construct eg a method! Unless we had two version ie a syn type version and a manual version, using either an enum or trait objects.
    methods.push((
        item_enum.ident.to_string(),
        false,
        JsFn {
            iife: false,
            public: false,
            export: false,
            async_: false,
            is_method: true,
            name: "constructor".to_string(),
            input_names: vec!["id".to_string(), "data".to_string()],
            body_stmts,
        },
    ));

    let mut static_fields = Vec::new();
    for variant in &item_enum.variants {
        static_fields.push(JsLocal {
            public: false,
            export: false,
            type_: LocalType::Static,
            lhs: LocalName::Single(format!("{}Id", camel(&variant.ident))),
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
                    lhs: LocalName::Single(format!(
                        "{}",
                        AsPascalCase(variant.ident.to_string()).to_string()
                    )),
                    value: JsExpr::New(
                        vec![class_name.clone()],
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
                // for thing in fields_named.named {
                //     let name = thing.ident.unwrap();
                // }
                // let stmt = JsStmt::Raw(format!(
                //     r#"return {{ id: "{}", data }};"#,
                //     variant.ident.to_string()
                // ));

                let stmt = JsStmt::Expr(
                    JsExpr::Return(Box::new(JsExpr::New(
                        vec![class_name.clone()],
                        vec![
                            JsExpr::LitStr(variant.ident.to_string()),
                            JsExpr::Var("data".to_string()),
                        ],
                    ))),
                    true,
                );
                (vec!["data".to_string()], vec![stmt])
            }
            syn::Fields::Unnamed(fields_unnamed) => {
                // const data = { id: "Baz" };
                // data.data = [text, num];
                // return data;
                let arg_names = fields_unnamed
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format!("arg_{i}"))
                    .collect::<Vec<_>>();

                let return_expr = JsExpr::Return(Box::new(JsExpr::New(
                    vec![class_name.clone()],
                    vec![
                        JsExpr::LitStr(variant.ident.to_string()),
                        JsExpr::Array(
                            arg_names
                                .iter()
                                .map(|name| JsExpr::Path(vec![name.clone()]))
                                .collect::<Vec<_>>(),
                        ),
                    ],
                )));
                (arg_names, vec![JsStmt::Expr(return_expr, true)])
            }
            syn::Fields::Unit => (Vec::new(), Vec::new()),
        };
        if body_stmts.len() > 0 {
            methods.push((
                item_enum.ident.to_string(),
                true,
                JsFn {
                    iife: false,
                    public: false,
                    export: false,
                    async_: false,
                    is_method: true,
                    name: variant.ident.to_string(),
                    input_names,
                    body_stmts,
                },
            ))
        }
    }

    // let module = global_data
    //     .modules
    //     .iter()
    //     .find(|m| &m.path == current_module)
    //     .unwrap();
    // let module_item_def = module
    //     .item_definitons
    //     .iter()
    //     .find(|item_def| item_def.ident == item_enum.ident.to_string());
    // let scoped_item_def = module.scoped_various_definitions.iter().find_map(|svd| {
    //     svd.1
    //         .item_definitons
    //         .iter()
    //         .find(|item_def| item_def.ident == item_enum.ident.to_string())
    // });
    // let item_def = scoped_item_def.or(module_item_def).unwrap().clone();
    // populate_fields_and_methods(
    //     global_data,
    //     current_module,
    //     &item_def,
    //     &generics,
    //     &mut methods,
    //     &mut static_fields,
    // );

    if let Some(dup) = global_data
        .duplicates
        .iter()
        .find(|dup| dup.name == class_name && &dup.original_module_path == current_module)
    {
        class_name = dup
            .namespace
            .iter()
            .map(|seg| camel(seg))
            .collect::<Vec<_>>()
            .join("__");
    }
    JsStmt::Class(JsClass {
        public: match item_enum.vis {
            Visibility::Public(_) => true,
            Visibility::Restricted(_) => todo!(),
            Visibility::Inherited => false,
        },
        export: false,
        tuple_struct: false,
        name: class_name,
        inputs: Vec::new(),
        static_fields,
        methods,
        rust_name: item_enum.ident.to_string(),
        is_impl_block: false,
        module_path: current_module.to_vec(),
        scope_id: global_data.scope_id_as_option(),
        // struct_or_enum: StructOrEnumSynObject::Enum(item_enum.clone()),
        // impld_methods: methods,
        // generic_trait_impl_methods: todo!(),
    })
}

pub fn handle_impl_item_fn(
    js_impl_items: &mut Vec<RustImplItemJs>,
    impl_item: &ImplItem,
    impl_item_fn: &ImplItemFn,
    global_data: &mut GlobalData,
    current_module_path: &[String],
    target_rust_type: &RustType,
    rust_impl_item: &RustImplItemNoJs,
) {
    let scope_count = {
        let scope_count = global_data.scope_count.last_mut().unwrap();
        *scope_count += 1;
        *scope_count
    };

    let static_ = match impl_item_fn.sig.inputs.first() {
        Some(FnArg::Receiver(_)) => false,
        _ => true,
    };
    // dbg!(item_impl_fn);
    // let private = !export;
    let js_input_names = impl_item_fn
        .clone()
        .sig
        .inputs
        .into_iter()
        .filter_map(|input| match input {
            FnArg::Receiver(_) => None,
            FnArg::Typed(pat_type) => match *pat_type.pat {
                Pat::Ident(pat_ident) => Some(camel(pat_ident.ident)),
                _ => todo!(),
            },
        })
        .collect::<Vec<_>>();

    // Get generics
    let mut fn_generics = impl_item_fn
        .sig
        .generics
        .params
        .iter()
        .map(|generic_param| match generic_param {
            GenericParam::Lifetime(_) => todo!(),
            GenericParam::Type(type_param) => {
                let name = type_param.ident.to_string();
                // let type_ = type_param
                //     .bounds
                //     .first()
                //     .map(|type_param_bound| {
                //         match get_return_type_of_type_param_bound(
                //             type_param_bound,
                //             &Vec::new(),
                //             current_module_path,
                //             &global_data,
                //         ) {
                //             RustType::ImplTrait => RustType::TypeParam(RustTypeParam {
                //                 name: name.clone(),
                //                 type_: RustTypeParamValue::Unresolved,
                //             }),
                //             RustType::TypeParam(_) => todo!(),
                //             RustType::Fn(return_type) => RustType::Fn(return_type),
                //             _ => todo!(),
                //         }
                //     })
                //     .unwrap_or(RustType::TypeParam(RustTypeParam {
                //         name: name.clone(),
                //         type_: RustTypeParamValue::Unresolved,
                //     }));
                RustTypeParam {
                    name,
                    type_: RustTypeParamValue::Unresolved,
                }
            }
            GenericParam::Const(_) => todo!(),
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

    // MORNING TODO lookup the impl item definition and use the parsed input types from that (which correctly pass FnOnce closure types) rather than parsing agains here.
    let mut vars = Vec::new();
    match &rust_impl_item.item {
        RustImplItemItemNoJs::Fn(_static, fn_info) => {
            for (is_self, is_mut, name, input_type) in fn_info.inputs_types.clone() {
                if is_self {
                    // TODO we need to ensure that RustType::Parent type is getting wrapped in RustType::MutRef where necessary

                    // NOTE THIS IS WRONG, we parsing the definition so the type params won't have changed from the first pass. It is once the method is *called* that we can attempt to further resolve type params eg the `T` in an `Option<T>`
                    let type_ = if is_mut {
                        // TODO does this mean self in `fn foo(mut self) {}` goes to RustType::MutRef??
                        RustType::MutRef(Box::new(target_rust_type.clone()))
                    } else {
                        target_rust_type.clone()
                    };

                    assert!(!is_mut);
                    let scoped_var = ScopedVar {
                        // TODO IMPORTANT surely this should be `self`???
                        // name: target_item.ident.clone(),
                        name: "self".to_string(),
                        // TODO how do we know if we have `foo(mut self)`?
                        mut_: false,
                        type_: input_type,
                    };
                    vars.push(scoped_var);
                } else {
                    // TODO what if input types contain type params that have been resolved? need a `fn update_type(possibly_resolved_generics, type)`
                    let scoped_var = ScopedVar {
                        name,
                        mut_: is_mut,
                        type_: input_type,
                    };
                    // dbg!(&scoped_var);
                    vars.push(scoped_var);
                }
            }
        }
        crate::RustImplItemItemNoJs::Const => todo!(),
    }

    // let mut fns = Vec::new();
    // record var and fn inputs
    for input in &impl_item_fn.sig.inputs {
        // match input {
        //     FnArg::Receiver(receiver) => {
        //         let type_ = if receiver.mutability.is_some() {
        //             RustType::MutRef(Box::new(target_rust_type.clone()))
        //         } else {
        //             target_rust_type.clone()
        //         };
        //         let scoped_var = ScopedVar {
        //             // TODO IMPORTANT surely this should be `self`???
        //             // name: target_item.ident.clone(),
        //             name: "self".to_string(),
        //             // TODO how do we know if we have `foo(mut self)`?
        //             mut_: false,
        //             type_,
        //         };
        //         vars.push(scoped_var);
        //     }
        //     FnArg::Typed(pat_type) => {
        //         let (ident, mut_) = match &*pat_type.pat {
        //             Pat::Ident(pat_ident) => {
        //                 (pat_ident.ident.to_string(), pat_ident.mutability.is_some())
        //             }
        //             _ => todo!(),
        //         };
        //         let rust_type = parse_fn_input_or_field(
        //             &*pat_type.ty,
        //             mut_,
        //             &fn_generics,
        //             current_module_path,
        //             global_data,
        //         );
        //         let scoped_var = ScopedVar {
        //             name: ident,
        //             mut_,
        //             type_: rust_type,
        //         };
        //         vars.push(scoped_var);
        //     }
        // }
    }

    // dbg!("handle_item_impl new scope");
    // println!("{}", quote! { #impl_item_fn });
    // Create scope for impl method/fn body
    info!("handle_item_impl new scope");
    // dbg!(&global_data.scopes);
    global_data.scope_count.push(0);
    global_data.scope_id.push(scope_count);

    global_data.scopes.push(GlobalDataScope {
        scope_id: global_data.scope_id.clone(),
        variables: vars,
        // fns: Vec::new(),
        // generics: fn_generics,
        // item_definitons: Vec::new(),
        look_in_outer_scope: false,
        // impl_blocks: Vec::new(),
        // trait_definitons: Vec::new(),
        // consts: Vec::new(),
        use_mappings: Vec::new(),
    });

    // TODO this approach for bool_and and add_assign is very limited and won't be possible if 2 differnt types need 2 different implementations for the same method name
    // TODO need to look up whether path is eg `rust_std::RustBool`, not just the item name
    // TODO see commented out code below this fn for old eg RustInteger + add_assign mappings/updates
    let n_stmts = impl_item_fn.block.stmts.len();
    let body_stmts = impl_item_fn
        .block
        .stmts
        .clone()
        .into_iter()
        .map(|stmt| stmt)
        .collect::<Vec<_>>();
    let returns_non_mut_ref_val = match &impl_item_fn.sig.output {
        ReturnType::Default => false,
        ReturnType::Type(_, type_) => match &**type_ {
            Type::Reference(_) => false,
            _ => true,
        },
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

    let body_stmts = parse_fn_body_stmts(
        false,
        returns_non_mut_ref_val,
        true,
        &body_stmts,
        global_data,
        current_module_path,
    );

    let body_stmts = Some(body_stmts);

    // TODO no idea why body_stmts is an `Option`
    if let Some((body_stmts, return_type)) = body_stmts {
        // push to rust_impl_items
        let fn_generics = impl_item_fn
            .sig
            .generics
            .params
            .iter()
            .filter_map(|gen| match gen {
                GenericParam::Lifetime(_) => None,
                GenericParam::Type(type_param) => Some(type_param.ident.to_string()),
                GenericParam::Const(_) => todo!(),
            })
            .collect::<Vec<_>>();
        let fn_rust_type_params = &fn_generics
            .iter()
            .map(|gen| RustTypeParam {
                name: gen.clone(),
                type_: RustTypeParamValue::Unresolved,
            })
            .collect::<Vec<_>>();
        let inputs_types = impl_item_fn
            .sig
            .inputs
            .iter()
            .map(|input| match input {
                FnArg::Receiver(_) => (true, false, "self".to_string(), RustType::ParentItem),
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
                    parse_fn_input_or_field(
                        &*pat_type.ty,
                        match &*pat_type.pat {
                            Pat::Ident(pat_ident) => pat_ident.mutability.is_some(),
                            _ => todo!(),
                        },
                        &fn_rust_type_params,
                        current_module_path,
                        global_data,
                    ),
                ),
            })
            .collect::<Vec<_>>();

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

        let fn_info = FnInfo {
            ident: impl_item_fn.sig.ident.to_string(),
            is_pub,
            inputs_types: inputs_types,
            generics: fn_generics,
            return_type: match &impl_item_fn.sig.output {
                ReturnType::Default => RustType::Unit,
                ReturnType::Type(_, type_) => parse_fn_input_or_field(
                    &*type_,
                    false,
                    &fn_rust_type_params,
                    current_module_path,
                    global_data,
                ),
            },
        };

        let js_fn = JsFn {
            iife: false,
            public: is_pub,
            export: false,
            // TODO
            async_: false,
            is_method: true,
            name: camel(impl_item_fn.sig.ident.clone()),
            input_names: js_input_names,
            body_stmts: body_stmts,
        };
        js_impl_items.push(RustImplItemJs {
            ident: impl_item_fn.sig.ident.to_string(),
            item: RustImplItemItemJs::Fn(static_, fn_info, js_fn),
            // syn_object: impl_item.clone(),
        });
    }
    info!("handle_item_impl after scope");
    // dbg!(&global_data.scopes);
    global_data.scope_count.pop();
    global_data.scope_id.pop();
    global_data.scopes.pop();
}

pub fn handle_item_impl(
    item_impl: &ItemImpl,
    at_module_top_level: bool,
    global_data: &mut GlobalData,
    current_module_path: &[String],
) -> Vec<JsStmt> {
    let debug_self_type = match &*item_impl.self_ty {
        Type::Path(type_path) => format!("{:?}", type_path.path.segments),
        _ => format!("{:?}", item_impl.self_ty),
    };
    let span = debug_span!("handle_item_impl", debug_self_type = ?debug_self_type);
    let _guard = span.enter();
    // dbg!(format!(
    //     "handle_impl_item: {:?}, {:?}",
    //     &item_impl.trait_, &item_impl.self_ty
    // ));

    let unique_id = get_item_impl_unique_id(
        current_module_path,
        &global_data.scope_id_as_option(),
        item_impl,
    );
    let impl_blocks = global_data
        .impl_blocks_simpl
        .iter()
        .filter(|ib| ib.unique_id == unique_id)
        .cloned()
        .collect::<Vec<_>>();

    let module = global_data
        .modules
        .iter()
        .find(|m| &m.path == current_module_path)
        .unwrap();

    // TODO most/all of this should exist on the `RustImplBlockSimple` so this is unncessary duplication
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
                                    .map(|seg| seg.ident.to_string())
                                    .collect::<Vec<_>>();
                                let (module_path, scope_id, trait_def) = global_data
                                    .lookup_trait_definition_any_module(
                                        current_module_path,
                                        &global_data.scope_id_as_option(),
                                        &trait_path,
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
                        generic.ident == type_path.path.segments.first().unwrap().ident.to_string()
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
        let (module_path, scope_id, trait_def) = global_data.lookup_trait_definition_any_module(
            current_module_path,
            &global_data.scope_id_as_option(),
            &trait_
                .segments
                .iter()
                .map(|seg| seg.ident.to_string())
                .collect::<Vec<_>>(),
        );
        (module_path, scope_id, trait_def.name)
    });

    // if let Some(trait_) = &item_impl.trait_ {
    //     if trait_.1.segments.len() != 1 {
    //         todo!()
    //     }
    //     global_data.default_trait_impls_class_mapping.push((
    //         target_item.ident.clone(),
    //         trait_.1.segments.first().unwrap().ident.to_string(),
    //     ));
    // }

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
            let opt_scope_id = global_data.scope_id_as_option();
            let (target_item_module, resolved_scope_id, target_item) = global_data
                .lookup_item_definition_any_module_or_scope(
                    current_module_path,
                    &opt_scope_id,
                    &impl_item_target_path,
                );

            // TODO seems like we are only handling cases like `impl<T> Foo<T>` and not `impl Foo<i32>`?
            let target_item_type_params = target_item
                .generics
                .iter()
                .map(|g| RustTypeParam {
                    name: g.clone(),
                    type_: RustTypeParamValue::Unresolved,
                })
                .collect::<Vec<_>>();

            if target_item_module == [PRELUDE_MODULE_PATH] {
                // NOTE we have called lookup_item_definition_any_module_or_scope which of course returns an ItemDefinition but this might be for eg `Option`, so we must check and return the correct RustType
                match target_item.ident.as_str() {
                    "Option" => {
                        assert_eq!(target_item_type_params.len(), 1);
                        (
                            RustType::Option(target_item_type_params.into_iter().next().unwrap()),
                            false,
                        )
                    }
                    _ => todo!(),
                }
            } else {
                (
                    RustType::StructOrEnum(
                        target_item_type_params,
                        target_item_module,
                        resolved_scope_id,
                        target_item.ident.to_string(),
                    ),
                    false,
                )
            }
        };

    global_data
        .impl_block_target_type
        .push(target_rust_type.clone());

    // let mut impl_stmts = Vec::new();
    let mut rust_impl_items = Vec::new();
    for impl_item in &item_impl.items {
        match impl_item {
            ImplItem::Const(impl_item_const) => {
                let js_local = JsLocal {
                    public: false,
                    export: false,
                    type_: LocalType::Static,
                    lhs: LocalName::Single(impl_item_const.ident.to_string()),
                    value: handle_expr(&impl_item_const.expr, global_data, &current_module_path).0,
                };

                rust_impl_items.push(RustImplItemJs {
                    ident: impl_item_const.ident.to_string(),
                    item: RustImplItemItemJs::Const(js_local),
                    // syn_object: impl_item.clone(),
                });
            }
            ImplItem::Fn(impl_item_fn) => {
                let rust_impl_item = impl_blocks
                    .iter()
                    .find_map(|impl_block| {
                        impl_block
                            .rust_items
                            .iter()
                            .find(|i| i.ident == impl_item_fn.sig.ident.to_string())
                    })
                    .unwrap();
                handle_impl_item_fn(
                    &mut rust_impl_items,
                    impl_item,
                    impl_item_fn,
                    global_data,
                    current_module_path,
                    &target_rust_type,
                    rust_impl_item,
                )
            }
            ImplItem::Type(_) => todo!(),
            ImplItem::Macro(_) => todo!(),
            ImplItem::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }

    let rust_impl_block = JsImplBlock2 {
        unique_id: unique_id.clone(),
        generics: rust_impl_block_generics,
        trait_: trait_path_and_name,
        target: target_rust_type.clone(),
        items: rust_impl_items
            .into_iter()
            .map(|x| (false, x))
            .collect::<Vec<_>>(),
    };

    // If the block gets pushed to `global_data.impl_blocks` then `update_clases()` should add the method to the appropriate class, however if the block is added to a scope then we need to do what `update_classes()` does here.

    // // TODO note that unlike other items, we push both scoped and module level impl blocks here, need to move them.
    // if !at_module_top_level {
    //     // a scoped impl block must at least be in the same scope or a child scope of any types used in the impl definition, ie the target/self type, the trait if it is a trait impl, and any types used in the generics of the target/self and trait. So we can/should hoist the impl block to the "lowest common denominator.
    //     // IMPORTANT NOTE This approach seems flawed given that the methods impl'd can be used in higher scopes than the impl.
    //     // Get lowest scope
    //     // NOTE we enumerate the scopes in reverse so that we can determine whether the found scope is the current scope (ie the impl is in the same scope as it's target and other definitions it uses)
    //     let scope = global_data
    //         .scopes
    //         .iter_mut()
    //         .rev()
    //         .enumerate()
    //         .find(|(i, s)| {
    //             // Is target type scoped?
    //             let is_target_item_scope = match &target_rust_type {
    //                 RustType::TypeParam(_) => {
    //                     // NOTE if target is a type param then is potentially applies to all type, in which case it seems to make sense to hoist to the module level *(or trait scope leve) (so that it's methods are accessible from everywhere) by doing `global_data.impl_blocks.push(rust_impl_block);` not `scope.impl_blocks.push(rust_impl_block);`
    //                     false
    //                 }
    //                 RustType::StructOrEnum(
    //                     _,
    //                     target_item_module,
    //                     target_item_scope,
    //                     target_item_name,
    //                 ) => {
    //                     if target_item_module.is_none() {
    //                         s.item_definitons
    //                             .iter()
    //                             .any(|item_def| &item_def.ident == target_item_name)
    //                     } else {
    //                         false
    //                     }
    //                 }
    //                 _ => todo!(),
    //             };
    //             let is_trait_scope = if let Some(trait_) = &item_impl.trait_ {
    //                 let trait_path = &trait_.1.segments;
    //                 if trait_path.len() == 1 {
    //                     s.trait_definitons.iter().any(|trait_def| {
    //                         trait_def.name == trait_path.first().unwrap().ident.to_string()
    //                     })
    //                 } else {
    //                     false
    //                 }
    //             } else {
    //                 false
    //             };
    //             // IMPORTANT TODO what about all the other types used in the impl'd items? We can't be in a higher scope than these without capturing them
    //             // let is_other_items_scope = ...
    //             // *NOTE even a `impl<T> Foo for T` method *cannot* be called in a parent scope of the trait, so it only makes sense to hoist the impl to the same scope as the trait (given `is_target_item_scope` will always be false in this case)

    //             is_target_item_scope || is_trait_scope
    //         });

    //     if let Some((scope_idx, scope)) = scope {
    //         if rust_impl_block.trait_.is_some() {
    //             // NOTE haven't though this through, just seeing if it works
    //             scope.impl_blocks.push(rust_impl_block.clone());
    //         } else {
    //             // Here we simply store the rust impl block on the appropriate scope, at the end of parsing a block of statements we will iterate through any `.impl_blocks` for the current scope and update the classes in the parsed stmts accordingly before returning the statements

    //             if scope_idx > 0 {
    //                 todo!()
    //             } else {
    //                 scope.impl_blocks.push(rust_impl_block.clone());
    //             }
    //         }
    //     } else {
    //         // If the types used are all module level, then we can hoist the impl block to module level
    //         global_data.impl_blocks.push(rust_impl_block.clone());
    //     }
    // } else {
    //     // TODO IMPORTANT what if the methods from this impl block are used before we've added the impl block to global_data.impl_blocks???
    //     global_data.impl_blocks.push(rust_impl_block.clone());
    // }

    global_data.impl_blocks.push(rust_impl_block.clone());

    global_data.impl_block_target_type.pop();

    let class_stmt = if is_target_type_param {
        let static_fields = rust_impl_block
            .items
            .iter()
            .cloned()
            .filter_map(|(used, item)| match item.item {
                RustImplItemItemJs::Fn(_, _, _) => None,
                RustImplItemItemJs::Const(js_local) => Some(js_local),
            })
            .collect::<Vec<_>>();
        let methods = rust_impl_block
            .items
            .iter()
            .cloned()
            .filter_map(|(used, item)| match item.item {
                RustImplItemItemJs::Fn(static_, fn_info, js_fn) => {
                    Some((rust_impl_block.js_name(), static_, js_fn))
                }
                RustImplItemItemJs::Const(_) => None,
            })
            .collect::<Vec<_>>();

        JsStmt::Class(JsClass {
            public: false,
            export: false,
            tuple_struct: false,
            name: rust_impl_block.js_name(),
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

    fn prelude_item_def_name_to_js(item_def_name: &String) -> &'static str {
        match item_def_name.as_str() {
            "i32" => "Number",
            "String" => "String",
            "str" => "String",
            // "bool" => "Boolean",
            "Bool" => "Boolean",
            "Box" => "donotuse",
            "Option" => "donotuse",
            "Vec" => "Array",
            _ => todo!(),
        }
    }
    let prelude_module = global_data
        .modules
        .iter()
        .find(|m| m.path == [PRELUDE_MODULE_PATH])
        .unwrap();
    let mut dedup_rust_prelude_definitions = prelude_module
        .item_definitons
        .iter()
        .cloned()
        .filter_map(|item_def| {
            let new_name = prelude_item_def_name_to_js(&item_def.ident);
            if new_name != "donotuse" {
                Some((new_name, item_def))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    dedup_rust_prelude_definitions.sort_by_key(|(js_name, item_def)| js_name.clone());
    dedup_rust_prelude_definitions.dedup_by_key(|(js_name, item_def)| js_name.clone());

    for (js_name, prelude_item_def) in &dedup_rust_prelude_definitions {
        if prelude_item_def.impl_block_ids.contains(&unique_id) {
            for (is_used, item) in &rust_impl_block.items {
                // TODO only add if `is_used == true`
                let item_name = camel(&item.ident);
                let block_name = &rust_impl_block.js_name();
                stmts.push(JsStmt::Raw(format!(
                    "{js_name}.prototype.{item_name} = {block_name}.prototype.{item_name}"
                )))
            }
        }
    }

    stmts
}

// if target_item.ident == "RustBool"
//                     && item_impl_fn.sig.ident == "bool_and"
//                 {
//                     Some((
//                         vec![JsStmt::Raw("this.jsBoolean && other.jsBoolean".to_string())],
//                         RustType::Bool,
//                     ))

//                     // fn add_assign(&mut self, other: RustInteger<T>) {
//                     //     self.js_number.0 += other.js_number.0;
//                     // }
//                 } else if target_item.ident == "RustInteger"
//                     && item_impl_fn.sig.ident == "add_assign"
//                 {
//                     Some((
//                         vec![JsStmt::Raw("this.jsNumber += other.inner()".to_string())],
//                         RustType::Todo,
//                     ))
//                 } else if target_item.ident == "RustInteger"
//                     && item_impl_fn.sig.ident == "deref_assign"
//                 {
//                     Some((
//                         vec![JsStmt::Raw("this.jsNumber = other.inner()".to_string())],
//                         RustType::Todo,
//                     ))
//                 } else if target_item.ident == "RustString"
//                     && item_impl_fn.sig.ident == "add_assign"
//                 {
//                     Some((
//                         vec![JsStmt::Raw("this.jsString += other.inner()".to_string())],
//                         RustType::Todo,
//                     ))
//                 } else if target_item.ident == "RustString" && item_impl_fn.sig.ident == "push_str"
//                 {
//                     Some((
//                         vec![JsStmt::Raw("this.jsString += other.jsString".to_string())],
//                         RustType::Todo,
//                     ))
//                 } else if target_item.ident == "RustString"
//                     && item_impl_fn.sig.ident == "deref_assign"
//                 {
//                     Some((
//                         vec![JsStmt::Raw("this.jsString = other.jsString".to_string())],
//                         RustType::Todo,
//                     ))
//                 } else if target_item.ident == "Option" && item_impl_fn.sig.ident == "eq" {
//                     let s = "return this.id === other.id && JSON.stringify(this.data) === JSON.stringify(other.data)";
//                     Some((vec![JsStmt::Raw(s.to_string())], RustType::Todo))
//                 } else if target_item.ident == "Option" && item_impl_fn.sig.ident == "ne" {
//                     Some((
//                         vec![JsStmt::Raw(
//                             "return this.id !== other.id || this.data.ne(other.data)".to_string(),
//                         )],
//                         RustType::Todo,
//                     ))
//                 } else if target_item.ident == "RustBool" && item_impl_fn.sig.ident == "eq" {
//                     Some((
//                         vec![JsStmt::Raw(
//                             "return this.jsBoolean === other.jsBoolean".to_string(),
//                         )],
//                         RustType::Todo,
//                     ))
//                 } else if target_item.ident == "RustBool" && item_impl_fn.sig.ident == "ne" {
//                     Some((
//                         vec![JsStmt::Raw(
//                             "return this.jsBoolean !== other.jsBoolean".to_string(),
//                         )],
//                         RustType::Todo,
//                     ))
//                 } else if target_item.ident == "RustString" && item_impl_fn.sig.ident == "clone" {
//                     Some((
//                         vec![JsStmt::Raw("return this.jsString".to_string())],
//                         RustType::Todo,
//                     ))
//                 }

pub fn handle_item_struct(
    item_struct: &ItemStruct,
    at_module_top_level: bool,
    global_data: &mut GlobalData,
    current_module_path: &[String],
) -> JsStmt {
    let mut name = item_struct.ident.to_string();
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

    // let generics = item_struct
    //     .generics
    //     .params
    //     .iter()
    //     .map(|p| match p {
    //         GenericParam::Lifetime(_) => todo!(),
    //         GenericParam::Type(type_param) => type_param.ident.to_string(),
    //         GenericParam::Const(_) => todo!(),
    //     })
    //     .collect::<Vec<_>>();

    // let generics_type_params = generics
    //     .iter()
    //     .map(|name| RustTypeParam {
    //         name: name.clone(),
    //         type_: RustTypeParamValue::Unresolved,
    //     })
    //     .collect::<Vec<_>>();

    // let fields = if item_struct.fields.len() == 0 {
    //     StructFieldInfo::UnitStruct
    // } else if item_struct.fields.iter().next().unwrap().ident.is_some() {
    //     StructFieldInfo::RegularStruct(
    //         item_struct
    //             .fields
    //             .iter()
    //             .map(|f| {
    //                 (
    //                     f.ident.as_ref().unwrap().to_string(),
    //                     parse_fn_input_or_field(
    //                         &f.ty,
    //                         // NOTE cannot make struct arg definitions mut
    //                         false,
    //                         &generics_type_params,
    //                         current_module_path,
    //                         global_data,
    //                     ),
    //                 )
    //             })
    //             .collect::<Vec<_>>(),
    //     )
    // } else {
    //     StructFieldInfo::TupleStruct(
    //         item_struct
    //             .fields
    //             .iter()
    //             .map(|f| {
    //                 parse_fn_input_or_field(
    //                     &f.ty,
    //                     false,
    //                     &generics_type_params,
    //                     current_module_path,
    //                     global_data,
    //                 )
    //             })
    //             .collect::<Vec<_>>(),
    //     )
    // };

    // Keep track of structs/enums in scope so we can subsequently add impl'd methods and then look up their return types when the method is called
    // NOTE we only push scoped definitions because module level definition are already pushed in extract_data_populate_item_definitions
    // if !at_module_top_level {
    //     let item_def = ItemDefinition {
    //         ident: item_struct.ident.to_string(),
    //         is_copy,
    //         generics,
    //         struct_or_enum_info: StructOrEnumDefitionInfo::Struct(StructDefinitionInfo {
    //             fields,
    //             syn_object: Some(item_struct.clone()),
    //         }),
    //         impl_blocks: Vec::new(),
    //     };
    //     let global_data_scope = global_data.scopes.last_mut().unwrap();
    //     global_data_scope.item_definitons.push(item_def.clone());
    // }

    // Populate methods and fields
    // TODO to avoid mut and immut ref clashing - fix this
    let mut methods = Vec::new();
    let static_fields = Vec::new();

    // let module = global_data
    //     .modules
    //     .iter()
    //     .find(|m| &m.path == current_module_path)
    //     .unwrap();
    // let module_item_def = module
    //     .item_definitons
    //     .iter()
    //     .find(|item_def| item_def.ident == item_struct.ident.to_string());
    // let scoped_item_def = module.scoped_various_definitions.iter().find_map(|svd| {
    //     svd.1
    //         .item_definitons
    //         .iter()
    //         .find(|item_def| item_def.ident == item_struct.ident.to_string())
    // });
    // let item_def = scoped_item_def.or(module_item_def).unwrap().clone();
    // populate_fields_and_methods(
    //     global_data,
    //     current_module_path,
    //     &item_def,
    //     &generics_type_params,
    //     &mut methods,
    //     &mut static_fields,
    // );

    if is_copy {
        let stmt = JsStmt::Raw("return JSON.parse(JSON.stringify(this));".to_string());
        methods.push((
            name.clone(),
            false,
            JsFn {
                iife: false,
                public: false,
                export: false,
                async_: false,
                is_method: true,
                name: "copy".to_string(),
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
            name.clone(),
            false,
            JsFn {
                iife: false,
                public: false,
                export: false,
                async_: false,
                is_method: true,
                name: "eq".to_string(),
                input_names: vec!["other".to_string()],
                body_stmts: vec![stmt],
            },
        ));
    }

    let mut js_name = name.clone();
    // dbg!(&global_data.duplicates);
    // dbg!(&name);
    // dbg!(&current_module_path);
    if at_module_top_level {
        if let Some(dup) = global_data
            .duplicates
            .iter()
            .find(|dup| dup.name == name && dup.original_module_path == *current_module_path)
        {
            js_name = dup
                .namespace
                .iter()
                .enumerate()
                .map(|(i, seg)| {
                    if i == dup.namespace.len() - 1 {
                        seg.clone()
                    } else {
                        camel(seg)
                    }
                })
                .collect::<Vec<_>>()
                .join("__");
        }
    }
    // dbg!(&js_name);

    let (tuple_struct, inputs) = match &item_struct.fields {
        Fields::Named(fields_named) => (
            false,
            fields_named
                .named
                .iter()
                .map(|field| camel(field.ident.as_ref().unwrap()))
                .collect::<Vec<_>>(),
        ),
        Fields::Unnamed(fields_unnamed) => (
            true,
            fields_unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, _field)| format!("arg{i}"))
                .collect::<Vec<_>>(),
        ),
        Fields::Unit => todo!(),
    };
    JsStmt::Class(JsClass {
        export: false,
        public: match item_struct.vis {
            Visibility::Public(_) => true,
            Visibility::Restricted(_) => todo!(),
            Visibility::Inherited => false,
        },
        name: js_name,
        tuple_struct,
        inputs,
        static_fields,
        methods,
        rust_name: item_struct.ident.to_string(),
        is_impl_block: false,
        module_path: current_module_path.to_vec(),
        scope_id: global_data.scope_id_as_option(),
    })
}

// pub fn handle_item(
//     item: Item,
//     // is_module: bool,
//     global_data: &mut GlobalData,
//     current_module_path: &mut Vec<String>,
//     js_stmts: &mut Vec<JsStmt>,
//     // current_file_path: &mut Option<PathBuf>,
// ) {

//     match item {
//         Item::Const(item_const) => {
//             js_stmts.push(handle_item_const(
//                 &item_const,
//                 true,
//                 global_data,
//                 current_module_path,
//             ));
//         }
//         Item::Enum(item_enum) => {
//             js_stmts.push(handle_item_enum(
//                 item_enum,
//                 true,
//                 global_data,
//                 current_module_path,
//             ));
//         }
//         Item::ExternCrate(_) => todo!(),
//         Item::Fn(item_fn) => {
//             js_stmts.push(handle_item_fn(
//                 &item_fn,
//                 true,
//                 global_data,
//                 current_module_path,
//             ));
//         }
//         Item::ForeignMod(_) => todo!(),
//         Item::Impl(item_impl) => {
//             js_stmts.push(handle_item_impl(
//                 &item_impl,
//                 true,
//                 global_data,
//                 current_module_path,
//             ));
//         }
//         Item::Macro(_) => todo!(),
//         Item::Mod(item_mod) => {
//             // NOTE in contrast to the other handlers here, handle_item_mod actually mutates `current_module_path` and appends a new JsModule to `global_data.transpiled_modules` instead of appending statements to `js_stmts`
//             handle_item_mod(
//                 item_mod,
//                 global_data,
//                 current_module_path,
//                 // current_file_path,
//             )
//         }
//         Item::Static(_) => todo!(),
//         Item::Struct(item_struct) => {
//             let js_stmt = handle_item_struct(&item_struct, true, global_data, current_module_path);
//             js_stmts.push(js_stmt);
//         }
//         Item::Trait(item_trait) => {
//             handle_item_trait(&item_trait, true, global_data, current_module_path);
//             js_stmts.push(JsStmt::Expr(JsExpr::Vanish, false));
//         }
//         Item::TraitAlias(_) => todo!(),
//         Item::Type(_) => todo!(),
//         Item::Union(_) => todo!(),
//         Item::Use(_) => {}
//         Item::Verbatim(_) => todo!(),
//         _ => todo!(),
//     }
// }

pub fn handle_item_mod(
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

    let items = if let Some(content) = &item_mod.content {
        // TODO how does `mod bar { mod foo; }` work?
        content.1.clone()
    } else {
        if let Some(crate_path) = &global_data.crate_path {
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
        }
    };

    // NOTE excluding use of attributes, only modules that are the directory parent can `mod foo`, any anywhere else we have to use `use` not `mod`.
    // In rust `mod foo` is largely redundant except for defining visibility and attributes https://stackoverflow.com/questions/32814653/why-is-there-a-mod-keyword-in-rust

    let current_module_path_copy = current_module_path.clone();
    let js_stmt_submodule = JsModule {
        public: match item_mod.vis {
            Visibility::Public(_) => true,
            Visibility::Restricted(_) => todo!(),
            Visibility::Inherited => false,
        },
        name: camel(item_mod.ident),
        module_path: current_module_path_copy.clone(),
        stmts: Vec::new(),
    };
    // convert from `syn` to `JsStmts`, passing the updated `current_file_path` to be used by any `mod` calls within the new module

    global_data.transpiled_modules.push(js_stmt_submodule);
    let stmts = js_stmts_from_syn_items(items, current_module_path, global_data);
    let js_stmt_module = global_data
        .transpiled_modules
        .iter_mut()
        .find(|tm| tm.module_path == current_module_path_copy)
        .unwrap();
    js_stmt_module.stmts = stmts;
    current_module_path.pop();

    // TODO shouldn't be using .export field as this is for importing from separate files. We don't want to add "export " to public values in a module, simply add them to the return statement of the function.
}

pub fn handle_item_trait(
    item_trait: &ItemTrait,
    at_module_top_level: bool,
    global_data: &mut GlobalData,
    current_module_path: &[String],
) {
    debug!("handle_item_trait");

    // NOTE we only push scoped definitions because module level definition are already pushed in extract_data_populate_item_definitions
    // if !at_module_top_level {
    //     let scope = global_data.scopes.last_mut().unwrap();
    //     scope.trait_definitons.push(RustTraitDefinition {
    //         name: item_trait.ident.to_string(),
    //     });
    // }

    // IMPORTANT TODO I think we need to be adding scoped traits to .scopes here but we are not
    for trait_item in &item_trait.items {
        match trait_item {
            TraitItem::Const(_) => todo!(),
            TraitItem::Fn(trait_item_fn) => {
                if let Some(default) = &trait_item_fn.default {
                    let js_fn = JsFn {
                        iife: false,
                        public: false,
                        export: false,
                        async_: false,
                        is_method: true,
                        name: camel(trait_item_fn.sig.ident.to_string()),
                        input_names: trait_item_fn
                            .sig
                            .inputs
                            .iter()
                            .filter_map(|input| match input {
                                FnArg::Receiver(_) => None,
                                FnArg::Typed(pat_type) => match &*pat_type.pat {
                                    Pat::Ident(pat_ident) => Some(camel(&pat_ident.ident)),
                                    _ => todo!(),
                                },
                            })
                            .collect::<Vec<_>>(),
                        body_stmts: default
                            .stmts
                            .iter()
                            .map(|stmt| {
                                handle_stmt(stmt, global_data, current_module_path)
                                    .into_iter()
                                    .map(|(stmt, type_)| stmt)
                            })
                            .flatten()
                            .collect::<Vec<_>>(),
                    };
                    global_data.default_trait_impls.push((
                        item_trait.ident.to_string(),
                        // TODO remove class name from JsImplItem::ClassMethod
                        JsImplItem::ClassMethod(
                            "shouldntneedclassnamehere".to_string(),
                            false,
                            match trait_item_fn.sig.inputs.first() {
                                Some(FnArg::Receiver(_)) => false,
                                Some(FnArg::Typed(_)) => true,
                                None => true,
                            },
                            js_fn,
                        ),
                    ));
                }
            }
            TraitItem::Type(_) => todo!(),
            TraitItem::Macro(_) => todo!(),
            TraitItem::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }
}
