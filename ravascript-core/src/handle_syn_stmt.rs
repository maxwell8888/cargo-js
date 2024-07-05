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
    camel, handle_item_use, handle_pat,
    handle_syn_expr::{handle_expr, handle_expr_and_stmt_macro},
    handle_syn_item::{
        handle_item_const, handle_item_enum, handle_item_fn, handle_item_impl, handle_item_struct,
        handle_item_trait,
    },
    js_ast::{JsExpr, JsIf, JsLocal, JsStmt, LocalName, LocalType},
    js_stmts_from_syn_items, parse_fn_body_stmts, parse_fn_input_or_field, ConstDef,
    EnumDefinitionInfo, EnumVariantInfo, EnumVariantInputsInfo, FnInfo, GlobalData,
    GlobalDataScope, ItemDefinition, ItemUseModuleOrScope, JsImplBlock2, JsImplItem, RustGeneric,
    RustImplItemItemJs, RustImplItemJs, RustTraitDefinition, RustType, RustTypeParam,
    RustTypeParamValue, ScopedVar, StructDefinitionInfo, StructFieldInfo, StructOrEnumDefitionInfo,
};

fn handle_local(
    local: &Local,
    global_data: &mut GlobalData,
    current_module_path: &[String],
) -> JsStmt {
    let span = debug_span!("handle_local", lhs = ?quote! { #local }.to_string());
    let _guard = span.enter();

    // TODO should also check:
    // by_ref: Some(
    //     Ref,
    // ),
    // mutability: Some(
    //     Mut,
    // ),

    // TODO a lot of this analysis is unneccessary and is recorded in the RustType now?
    // Check if rhs is &mut or * and if so remove &mut or * from expr
    // let (rhs_takes_mut_ref, rhs_is_deref, rhs_expr) = match &*local.init.as_ref().unwrap().expr {
    //     Expr::Reference(expr_ref) => (expr_ref.mutability.is_some(), false, *expr_ref.expr.clone()),
    //     Expr::Unary(expr_unary) => match expr_unary.op {
    //         UnOp::Deref(_) => (false, true, *expr_unary.expr.clone()),
    //         _ => (false, false, *local.init.as_ref().unwrap().expr.clone()),
    //     },
    //     _ => (false, false, *local.init.as_ref().unwrap().expr.clone()),
    // };

    // if rhs is fn call then get the return type of the fn
    // I'm assuming this can also be *or* a method call
    // TODO I don't know why we are getting the return type here, surely we should be calculating the return type when the rhs is parsed?
    // let rhs_is_call = match &rhs_expr {
    //     Expr::Call(expr_call) => true,
    //     Expr::If(_) => todo!(),
    //     Expr::Lit(_) => false,
    //     Expr::Macro(_) => todo!(),
    //     Expr::Match(_) => todo!(),
    //     Expr::MethodCall(expr_method_call) => true,
    //     Expr::Paren(_) => todo!(),
    //     Expr::Path(expr_path) => {
    //         // if expr_path.path.segments.len() == 1 {
    //         //     global_data.scopes.last().unwrap().0.iter().rev().any(
    //         //         |ScopedVar { name, mut_ref, .. }| {
    //         //             let lookup_varname =
    //         //                 expr_path.path.segments.first().unwrap().ident.to_string();
    //         //             *name == lookup_varname && *mut_ref
    //         //         },
    //         //     )
    //         // } else {
    //         //     todo!()
    //         // }
    //         false
    //     }
    //     Expr::Reference(_) => todo!(),
    //     Expr::Unary(_) => todo!(),
    //     Expr::Struct(_) => false,
    //     _ => {
    //         dbg!(rhs_expr);
    //         todo!()
    //     }
    // };

    // If `var mut num = 1;` or `var num = &mut 1` or `var mut num = &mut 1` then wrap num literal in RustInteger or RustFLoat
    // what if we have a fn returning an immutable integer which is then getting made mut or &mut here? or a field or if expression or parens or block or if let or match or method call or ... . We just check for each of those constructs, and analyse them to determine the return type? Yes but this is way easier said than done so leave it for now but start record var type info as a first step towards being able to do this analysis.
    // determining types
    // easy: fn calls, method calls, fields,
    // hard: if expression, parens, block, if let, match, method call

    // If rhs is a non &mut var which is a `Copy` type, we need to add `.copy()`.
    // Other values eg something returned from a fn don't need to be copied because we will no longer have access to the original value to mutate it
    // Other values like a var field which is an object we do want to `.copy()`

    // NOTE rhs expr is an option because it is possible to create an unitialised var in Rust like `let foo;` but we will no support this (for now)
    let Some(local_init) = &local.init else {
        panic!("uninitialized variables not supported")
    };

    // dbg!("handle_local");
    // println!("{}", quote! { #local });
    let (mut rhs_expr, rhs_type) = handle_expr(&local_init.expr, global_data, current_module_path);

    // NOTE we must calculate lhs_is_shadowing before calling `handle_pat(&local.pat...` because handle pat adds the current var to the scope
    let lhs_is_shadowing = global_data
        .scopes
        .last()
        .unwrap()
        .variables
        .iter()
        .any(|var| {
            fn var_name_matches(pat: &Pat, var_name: &String) -> bool {
                match pat {
                    // TODO determine whether lhs is shadowing for `Pat`s other than `Pat::Ident`
                    Pat::Ident(pat_ident) => pat_ident.ident == var_name,
                    Pat::Slice(_) => false,
                    Pat::Struct(_) => false,
                    Pat::Type(pat_type) => var_name_matches(&*pat_type.pat, var_name),
                    Pat::Wild(_) => false,
                    other => {
                        dbg!(other);
                        todo!();
                    }
                }
            }
            var_name_matches(&local.pat, &var.name)
        });

    // TODO handle case where type annotation contains a type param eg `let foo: Option<T> = None;`
    let type_annotation = match &local.pat {
        Pat::Type(pat_type) => {
            let is_mut = match &*pat_type.pat {
                Pat::Ident(pat_ident) => pat_ident.mutability.is_some(),
                _ => todo!(),
            };
            Some(parse_fn_input_or_field(
                &pat_type.ty,
                is_mut,
                // parent_item_definition_generics,
                // TODO update parse_fn_input_or_field to handle type params in the scope eg for `let foo: Option<T> = None;` the T will be defined eg on the fn and may have already been resolved
                &Vec::new(),
                current_module_path,
                global_data,
            ))
        }
        _ => None,
    };

    // NOTE handle_pat pushes the var to the current scope
    let lhs = handle_pat(
        &local.pat,
        global_data,
        // TODO handle case where the type annotation contains type params that are actually resolved in rhs_type
        type_annotation.unwrap_or(rhs_type.clone()),
    );

    // If lhs is `mut` and rhs is a JS primative then we need to wrap it in eg `new RustInteger()`. If rhs is already a `mut` JS primative, it needs copying.
    let lhs_is_mut = match &local.pat {
        Pat::Ident(pat_ident) => pat_ident.mutability.is_some(),
        _ => false,
    };

    // NOTE need to consider whether lhs is destructuring given `let foo = some_number_array;` needs `.copy()`ing whereas `let [one, two] = some_number_array;` doesn't.
    let mut handle_should_add_copy_expr_path = |expr_path: &ExprPath| {
        if expr_path.path.segments.len() == 1 {
            let scopes_clone = global_data.scopes.clone();
            let var = scopes_clone.iter().rev().find_map(|s| {
                s.variables
                    .iter()
                    .find(|v| v.name == expr_path.path.segments.first().unwrap().ident.to_string())
            });
            if let Some(var) = var {
                assert!(var.type_ == rhs_type);

                // Get var item def so we can check if var is a copy struct or an array
                fn handle_type(
                    // NOTE We need to specify the RustType separately to the ScopedVar to allow passing in the inner type of a RustType::MutRef
                    rust_type: &RustType,
                    global_data: &mut GlobalData,
                    var: &ScopedVar,
                    lhs: &LocalName,
                    mut_ref_taken: bool,
                ) -> bool {
                    match rust_type {
                        RustType::Unit => todo!(),
                        RustType::Never => todo!(),
                        RustType::ImplTrait(_) => todo!(),
                        RustType::TypeParam(_) => todo!(),
                        RustType::I32 => false,
                        RustType::F32 => todo!(),
                        RustType::Bool => todo!(),
                        RustType::String => todo!(),
                        RustType::Option(_) => todo!(),
                        RustType::Result(_) => todo!(),
                        RustType::StructOrEnum(type_params, module_path, scope_id, name) => {
                            let item_def = global_data
                                .lookup_item_def_known_module_assert_not_func2(
                                    module_path,
                                    scope_id,
                                    name,
                                );
                            item_def.is_copy && var.mut_ && !mut_ref_taken
                        }
                        RustType::Vec(_) => todo!(),
                        RustType::Array(element_type) => {
                            // (although this won't be necessary if the previous value is not used after the move/copy, but this would be hard to determine so need to just always add copy)
                            match lhs {
                                LocalName::Single(_) => true,
                                LocalName::DestructureObject(_) => todo!(),
                                LocalName::DestructureArray(_) => {
                                    // If element_type is `Copy` struct/enum then should add `.copy()` to rhs array
                                    match &**element_type {
                                        RustType::StructOrEnum(
                                            type_params,
                                            module_path,
                                            scope_id,
                                            name,
                                        ) => {
                                            let item_def = global_data
                                                .lookup_item_def_known_module_assert_not_func2(
                                                    module_path,
                                                    &global_data.scope_id_as_option(),
                                                    name,
                                                );
                                            if item_def.is_copy {
                                                global_data.rust_prelude_types.rust_array_copy =
                                                    true;
                                                true
                                            } else {
                                                false
                                            }
                                        }
                                        _ => false,
                                    }
                                }
                            }
                        }
                        RustType::Tuple(_) => todo!(),
                        RustType::UserType(_, _) => todo!(),
                        RustType::MutRef(inner) => {
                            // A &mut T is always `Copy` copied (not the T, the reference) so don't need to `.copy()` it, unless it is dereferenced - *or destructured*
                            handle_type(&*inner, global_data, var, lhs, true)
                        }
                        RustType::Ref(_) => todo!(),
                        RustType::Fn(_, _, _, _, _) => todo!(),
                        RustType::Closure(_, _) => todo!(),
                        _ => todo!(),
                    }
                }
                handle_type(&var.type_, global_data, var, &lhs, false)
            } else {
                false
            }
        } else {
            false
        }
    };
    let rhs_should_add_copy = match &*local_init.expr {
        Expr::Async(_) => todo!(),
        Expr::Await(_) => todo!(),
        // TODO not sure if this is correct, need to add some tests for objects that need `.copy()`ing and are returned from blocks
        Expr::Block(_) => false,
        Expr::Cast(_) => todo!(),
        Expr::Const(_) => todo!(),
        Expr::Continue(_) => todo!(),
        Expr::Field(_) => todo!(),
        Expr::ForLoop(_) => todo!(),
        Expr::Group(_) => todo!(),
        Expr::Index(_) => todo!(),
        Expr::Infer(_) => todo!(),
        Expr::Let(_) => todo!(),
        Expr::Loop(_) => todo!(),
        Expr::Paren(_) => todo!(),
        Expr::Path(expr_path) => handle_should_add_copy_expr_path(expr_path),
        Expr::Range(_) => todo!(),
        Expr::Reference(expr_reference) => {
            if expr_reference.mutability.is_some() {
                match &*expr_reference.expr {
                    // Expr::Path(expr_path) => {
                    //     handle_should_add_copy_expr_path(expr_path)
                    // }
                    _ => false,
                }
            } else {
                false
            }
        }
        Expr::Repeat(_) => todo!(),
        Expr::Return(_) => todo!(),
        Expr::Try(_) => todo!(),
        Expr::TryBlock(_) => todo!(),
        Expr::Unary(_) => false,
        Expr::Unsafe(_) => todo!(),
        Expr::Verbatim(_) => todo!(),
        Expr::While(_) => todo!(),
        Expr::Yield(_) => todo!(),
        _ => false,
    };
    // TODO We have two `rhs_should_add_copy`s because for some cases it is easier to check the syn expr, in some cases it is easier/necessary to look at the parsed RustType. Ideally we would store sufficient info on the RustType to combine these??
    // let rhs_should_add_copy2 = match &rhs_type {
    //     ...
    // };
    // if rhs_should_add_copy || rhs_should_add_copy2 {
    if rhs_should_add_copy {
        rhs_expr = JsExpr::MethodCall(Box::new(rhs_expr), "copy".to_string(), Vec::new());
    }

    if lhs_is_mut && rhs_type.is_js_primative() {
        rhs_expr = match rhs_type {
            RustType::NotAllowed => todo!(),
            RustType::Unknown => todo!(),
            RustType::Todo => todo!(),
            RustType::ParentItem => todo!(),
            RustType::Unit => todo!(),
            RustType::Never => todo!(),
            RustType::ImplTrait(_) => todo!(),
            RustType::TypeParam(_) => todo!(),
            RustType::I32 => {
                global_data.rust_prelude_types.rust_integer = true;
                JsExpr::New(vec!["RustInteger".to_string()], vec![rhs_expr])
            }
            RustType::F32 => todo!(),
            RustType::Bool => todo!(),
            RustType::String => {
                global_data.rust_prelude_types.rust_string = true;
                JsExpr::New(vec!["RustString".to_string()], vec![rhs_expr])
            }
            RustType::Option(_) => todo!(),
            RustType::Result(_) => todo!(),
            RustType::StructOrEnum(_, _, _, _) => todo!(),
            RustType::Vec(_) => todo!(),
            RustType::Array(_) => todo!(),
            RustType::Tuple(_) => todo!(),
            RustType::UserType(_, _) => todo!(),
            RustType::MutRef(_) => rhs_expr,
            RustType::Ref(_) => todo!(),
            RustType::Fn(_, _, _, _, _) => todo!(),
            RustType::Closure(_, _) => rhs_expr,
            RustType::FnVanish => todo!(),
            RustType::Box(_) => todo!(),
        }
    }

    match rhs_expr {
        JsExpr::If(js_if) => {
            // TODO currently cases where the branch scope has a var with the same name as the result var means that the result will get assigned to that var, not the result var. Need to consider how to handle this. putting the branch lines inside a new `{}` scope and then doing the result assignment outside of this would work, but is ugly so would want to only do it where necessary, which would require iterating over the lines in a block to check for local declarations with that name.
            JsStmt::Expr(
                JsExpr::If(JsIf {
                    assignment: Some(lhs),
                    declare_var: true,
                    condition: js_if.condition,
                    succeed: js_if.succeed,
                    fail: js_if.fail,
                }),
                true,
            )
        }

        rhs => {
            if lhs_is_shadowing {
                JsStmt::Expr(
                    JsExpr::Assignment(
                        Box::new(match lhs {
                            LocalName::Single(name) => JsExpr::Path(vec![name]),
                            LocalName::DestructureObject(_) => todo!(),
                            LocalName::DestructureArray(_) => todo!(),
                        }),
                        Box::new(rhs),
                    ),
                    true,
                )
            } else {
                JsStmt::Local(JsLocal {
                    public: false,
                    export: false,
                    type_: LocalType::Let,
                    lhs,
                    value: rhs,
                })
            }
        }
    }

    // Add .copy() if rhs is a mut...
    // and rhs is `Copy`
    // dbg!(&lhs);
    // dbg!(&global_data.scopes);

    // TODO avoid using cloned here
    // let rhs_is_found_var = global_data
    //     .scopes
    //     .last()
    //     .unwrap()
    //     .variables
    //     .iter()
    //     .cloned()
    //     .rev()
    //     .find(|ScopedVar { name, mut_, .. }| match &rhs_expr {
    //         Expr::Path(expr_path) => {
    //             if expr_path.path.segments.len() == 1 {
    //                 expr_path.path.segments.first().unwrap().ident == name
    //             } else {
    //                 false
    //             }
    //         }
    //         _ => false,
    //     });

    // dbg!(&rhs_is_found_var);

    // TODO
    // let rhs_is_deref_mut_ref = ...

    // let rhs_is_var = match &*local.init.as_ref().unwrap().expr {
    //     Expr::Path(expr_path) => expr_path.path.segments.len() == 1,
    //     _ => false,
    // };

    // Get var info: Expr -> TypeOrVar
    // ie:
    // Expr::Call -> TypeOrVar::Var(ScopedVar { name: "donotuse".to_string(), mut_: false, mut_ref: type_reference.mutability.is_some(), type_, })
    // Expr::Lit -> TypeOrVar::RustType
    // Expr::Path -> TypeOrVar::Var
    // let mut rhs_type = get_type_from_expr(&rhs_expr, global_data, current_module_path);
    // TODO the RustType::MutRef should be added by hande_expr(), not here
    // if rhs_takes_mut_ref {
    //     rhs_type = RustType::MutRef(Box::new(rhs_type));
    // }

    // rhs is a fn call that returns a &mut
    // TODO only handles fns defined in scope, not at top level
    // let fn_call_mut_ref = match &*local.init.as_ref().unwrap().expr {};

    // Record name if creating a mut or &mut variable
    // match &local.pat {
    //     Pat::Ident(pat_ident) => {
    //         if rhs_is_mut_ref || pat_ident.mutability.is_some() || fn_call_mut_ref {
    //             global_data.scopes.last_mut().unwrap().0.push(ScopedVar {
    //                 name: pat_ident.ident.to_string(),
    //                 mut_: pat_ident.mutability.is_some(),
    //                 mut_ref: rhs_is_mut_ref || fn_call_mut_ref,
    //                 type_: rust_type,
    //             });
    //         }
    //     }
    //     _ => {}
    // }

    // match &lhs {
    //     LocalName::Single(var_name) => {
    //         if is_mut_ref || local.init.
    //         global_data.vars_in_scope.push((var_name, ))
    //     },
    //     // TODO handle_pat needs to capture whether destructured variables are mut
    //     LocalName::DestructureObject(_) => {}
    //     LocalName::DestructureArray(_) => {}
    // }

    // dbg!(&lhs);
    // dbg!(&rhs);
    // dbg!(&rhs_expr);
    // dbg!(rhs_is_deref);
    // dbg!(&rhs_is_fn_call);
    // dbg!(rhs_is_found_var);
    // dbg!(rhs_takes_mut_ref);
    // dbg!(lhs_is_mut);

    // For now we will comment out all of this but keep it here for reference as we rebuild it with better notes and making better use of RustType
    // // NOTE this logic could be simplified a lot, but we have kept it verbose so that we can explain how each different case is handled. Maybe also have a simplified version and run both during tests but only the simplified version for prod, since it will be faster and also potentially easier to understand in some respect.
    // if !rhs_is_deref
    //     && !rhs_takes_mut_ref
    //     && !rhs_is_call
    //     && rhs_is_found_var.is_none()
    //     && !lhs_is_mut
    // {
    //     // normal primative copy ie `let num = 5; let num2 = num` - do nothing. But what about the `rhs_is_found_var.is_none()`? Surely that means we can't do `let num2 = num`???
    // } else if rhs_takes_mut_ref
    //     && rhs_is_found_var
    //         .as_ref()
    //         .map(|v| v.mut_ && !v.is_mut_ref())
    //         .unwrap_or(false)
    //     && !rhs_is_deref
    // {
    //     // take mut ref of mut var ie `let mut num = 5; let num2 = &mut num` - do nothing
    // } else if rhs_is_deref
    //     && rhs_is_found_var
    //         .as_ref()
    //         .map(|v| v.is_mut_ref())
    //         .unwrap_or(false)
    // {
    //     if lhs_is_mut {
    //         {
    //             let num = &mut 5; // or let mut num = &mut 5;
    //             let mut copy = *num;
    //         }
    //         rhs = JsExpr::MethodCall(Box::new(rhs), "copy".to_string(), vec![]);
    //     } else {
    //         {
    //             let num = &mut 5; // or let mut num = &mut 5;
    //             let copy = *num;
    //         }
    //         rhs = JsExpr::MethodCall(Box::new(rhs), "inner".to_string(), vec![]);
    //     }
    // }
    // // copy rhs if is mut and is a variable, which is being assigned
    // else if rhs_is_found_var
    //     .as_ref()
    //     .map(|v| v.mut_ && !v.is_mut_ref())
    //     .unwrap_or(false)
    // {
    //     if lhs_is_mut {
    //         {
    //             // I don't understad how these examples are `rhs_is_found_var`?????
    //             let mut num = 5;
    //             let mut copy = 5;
    //         }
    //         rhs = JsExpr::MethodCall(Box::new(rhs), "copy".to_string(), vec![]);
    //     } else {
    //         {
    //             let mut num = 5;
    //             let copy = 5;
    //         }
    //         rhs = JsExpr::MethodCall(Box::new(rhs), "inner".to_string(), vec![]);
    //     }
    //     // TODO for now just assume that any call being dereferenced returns a &mut and should be `.copy()`d. I don't think this will cause any incorrect behavior, only unnecessary copying, eg where the return is `&i32` not `&mut i32`
    // } else if rhs_is_call {
    //     if rhs_is_deref {
    //         if lhs_is_mut {
    //             // let mut copy = *some_fn();
    //             rhs = JsExpr::MethodCall(Box::new(rhs), "copy".to_string(), vec![]);
    //         } else {
    //             // let copy = *some_fn();
    //             rhs = JsExpr::MethodCall(Box::new(rhs), "inner".to_string(), vec![]);
    //         }
    //     } else {
    //         match rhs_type {
    //             RustType::Todo => todo!(),
    //             RustType::Unit => {}
    //             RustType::I32 => {
    //                 // fn returns i32
    //                 if lhs_is_mut {
    //                     // let mut copy = some_fn() -> i32;
    //                     global_data.rust_prelude_types.integer = true;
    //                     rhs = JsExpr::New(vec!["RustInteger".to_string()], vec![rhs]);
    //                 } else {
    //                     // let copy = some_fn() -> i32;
    //                     // do nothing
    //                 }
    //             }
    //             RustType::F32 => todo!(),
    //             RustType::Bool => todo!(),
    //             RustType::String => todo!(),
    //             RustType::StructOrEnum(_, _, _) => {
    //                 // TODO what do we need to do here? check if the struct/enum is `Copy` and clone it if so?
    //             }
    //             // RustType::Enum(_,_,_) => {}
    //             RustType::NotAllowed => todo!(),
    //             RustType::Unknown => todo!(),
    //             RustType::Never => todo!(),
    //             RustType::Vec(_) => todo!(),
    //             RustType::Array(_) => todo!(),
    //             RustType::Tuple(_) => todo!(),
    //             RustType::MutRef(ref rust_type) => {
    //                 match &**rust_type {
    //                     RustType::NotAllowed => todo!(),
    //                     RustType::Unknown => todo!(),
    //                     RustType::Todo => todo!(),
    //                     RustType::Unit => todo!(),
    //                     RustType::Never => todo!(),
    //                     RustType::I32 => {
    //                         // fn returns &mut i32
    //                         if rhs_is_deref {
    //                             if lhs_is_mut {
    //                                 // let mut copy = *some_fn() -> &mut i32;
    //                                 rhs = JsExpr::MethodCall(
    //                                     Box::new(rhs),
    //                                     "copy".to_string(),
    //                                     vec![],
    //                                 );
    //                             } else {
    //                                 // let copy = *some_fn() -> &mut i32;
    //                                 rhs = JsExpr::MethodCall(
    //                                     Box::new(rhs),
    //                                     "inner".to_string(),
    //                                     vec![],
    //                                 );
    //                             }
    //                         } else {
    //                             // lhs_is_mut is irrelevant
    //                             // let some_ref = some_fn() -> &mut i32;
    //                             // Do nothing because we are just assigning the mut ref to a new variable
    //                         }
    //                     }
    //                     RustType::F32 => todo!(),
    //                     RustType::Bool => todo!(),
    //                     RustType::String => todo!(),
    //                     RustType::StructOrEnum(_, _, _) => todo!(),
    //                     // RustType::Enum(_,_,_) => todo!(),
    //                     RustType::Vec(_) => todo!(),
    //                     RustType::Array(_) => todo!(),
    //                     RustType::Tuple(_) => todo!(),
    //                     RustType::MutRef(_) => todo!(),
    //                     RustType::Fn(_, _, _, _) => todo!(),
    //                     RustType::Option(_) => todo!(),
    //                     RustType::Result(_) => todo!(),
    //                     RustType::TypeParam(_) => todo!(),
    //                     RustType::ImplTrait(_) => todo!(),
    //                     RustType::ParentItem => todo!(),
    //                     RustType::UserType(_, _) => todo!(),
    //                     RustType::Ref(_) => todo!(),
    //                 }
    //             }
    //             RustType::Fn(_, _, _, _) => todo!(),
    //             RustType::Option(_) => todo!(),
    //             RustType::Result(_) => todo!(),
    //             RustType::TypeParam(_) => todo!(),
    //             RustType::ImplTrait(_) => todo!(),
    //             RustType::ParentItem => todo!(),
    //             RustType::UserType(_, _) => todo!(),
    //             RustType::Ref(_) => todo!(),
    //         }
    //     }
    // }
    // // Creating a mut/&mut var for a literal eg `let num = &mut 5` or `let mut num = 5;`
    // else if (rhs_takes_mut_ref || lhs_is_mut) && !rhs_is_deref && rhs_is_found_var.is_none() {
    //     {
    //         let num = &mut 5;
    //         // or
    //         let mut num = 5;
    //     }

    //     match &rhs_expr {
    //         Expr::Lit(expr_lit) => match &expr_lit.lit {
    //             Lit::Str(lit_str) => {
    //                 global_data.rust_prelude_types.string = true;
    //                 rhs = JsExpr::New(
    //                     vec!["RustString".to_string()],
    //                     vec![JsExpr::LitStr(lit_str.value())],
    //                 );
    //             }
    //             Lit::ByteStr(_) => {}
    //             Lit::Byte(_) => {}
    //             Lit::Char(_) => {}
    //             Lit::Int(lit_int) => {
    //                 global_data.rust_prelude_types.integer = true;
    //                 rhs = JsExpr::New(
    //                     vec!["RustInteger".to_string()],
    //                     vec![JsExpr::LitInt(lit_int.base10_parse::<i32>().unwrap())],
    //                 );
    //             }
    //             Lit::Float(_) => {}
    //             Lit::Bool(lit_bool) => {
    //                 global_data.rust_prelude_types.bool = true;
    //                 rhs = JsExpr::New(
    //                     vec!["RustBool".to_string()],
    //                     vec![JsExpr::LitBool(lit_bool.value)],
    //                 )
    //             }
    //             Lit::Verbatim(_) => {}
    //             _ => {}
    //         },
    //         // Expr::Path(_) => {
    //         //     if let Some(rhs_is_found_var) = rhs_is_found_var {
    //         //         if rhs_is_found_var.mut_ || rhs_is_found_var.mut_ref {
    //         //             match rhs_is_found_var.type_ {

    //         //             }
    //         //         }
    //         //     }
    //         // }
    //         _ => {}
    //     }
    // }
    // // Creating a mut/&mut var for a var eg `let num2 = &mut num` or `let mut num2 = num;`
    // else if ((lhs_is_mut
    //     && rhs_is_found_var
    //         .as_ref()
    //         .map(|v| !v.is_mut_ref())
    //         .unwrap_or(false))
    //     || (rhs_takes_mut_ref
    //         && rhs_is_found_var
    //             .as_ref()
    //             .map(|v| v.mut_ && !v.is_mut_ref())
    //             .unwrap_or(false)))
    //     && !rhs_is_deref
    // {
    //     {
    //         let num = 5;
    //         let mut copy = num;
    //     }
    //     {
    //         let mut num = 5;
    //         let copy = &mut num;
    //     }
    //     match rhs_is_found_var.unwrap().type_ {
    //         RustType::Todo => {}
    //         RustType::Unit => {}
    //         RustType::I32 => {
    //             global_data.rust_prelude_types.integer = true;
    //             rhs = JsExpr::New(vec!["RustInteger".to_string()], vec![rhs]);
    //         }
    //         RustType::F32 => {}
    //         RustType::Bool => {
    //             global_data.rust_prelude_types.bool = true;
    //             rhs = JsExpr::New(vec!["RustBool".to_string()], vec![rhs])
    //         }
    //         RustType::String => {
    //             global_data.rust_prelude_types.string = true;
    //             rhs = JsExpr::New(vec!["RustString".to_string()], vec![rhs]);
    //         }
    //         RustType::StructOrEnum(_, _, _) => {}
    //         // RustType::Enum(_,_,_) => {}
    //         RustType::NotAllowed => {}
    //         RustType::Unknown => {}
    //         RustType::Never => {}
    //         RustType::Vec(_) => {}
    //         RustType::Array(_) => {}
    //         RustType::Tuple(_) => {}
    //         RustType::MutRef(_) => {}
    //         RustType::Fn(_, _, _, _) => {}
    //         RustType::Option(_) => {}
    //         RustType::Result(_) => {}
    //         RustType::TypeParam(_) => {}
    //         RustType::ImplTrait(_) => {}
    //         RustType::ParentItem => todo!(),
    //         RustType::UserType(_, _) => todo!(),
    //         RustType::Ref(_) => todo!(),
    //     }
    // } else {
    //     dbg!(lhs);
    //     dbg!(rhs);
    //     dbg!(rhs_is_deref);
    //     dbg!(rhs_is_call);
    //     dbg!(rhs_is_found_var);
    //     dbg!(rhs_takes_mut_ref);
    //     dbg!(lhs_is_mut);
    //     dbg!(&global_data.scopes);
    //     todo!()
    // }

    // Record var info

    // match &local.pat {
    //     Pat::Ident(pat_ident) => {
    //         let mut scoped_var = ScopedVar {
    //             name: pat_ident.ident.to_string(),
    //             mut_: pat_ident.mutability.is_some(),
    //             // mut_ref: rhs_is_mut_ref || fn_call_mut_ref,
    //             // mut_ref: rhs_takes_mut_ref
    //             //     || (rhs_is_found_var.map(|var| var.mut_ref).unwrap_or(false) && !rhs_is_deref),
    //             type_: rhs_type,
    //         };
    //         // let mut scoped_var = ScopedVar {
    //         //     name: pat_ident.ident.to_string(),
    //         //     mut_: pat_ident.mutability.is_some(),
    //         //     // mut_ref: rhs_is_mut_ref || fn_call_mut_ref,
    //         //     mut_ref: rhs_takes_mut_ref
    //         //         || (rhs_is_found_var.map(|var| var.mut_ref).unwrap_or(false) && !rhs_is_deref),
    //         //     type_: rhs_type_or_var,
    //         // };
    //         // match rhs_type_or_var {
    //         // TypeOrVar::RustType(rust_type) => scoped_var.type_ = rust_type,
    //         // TypeOrVar::Var(found_var) => {
    //         //     scoped_var.mut_ref = scoped_var.mut_ref || found_var.mut_ref;
    //         //     scoped_var.type_ = found_var.type_;
    //         // }
    //         // TypeOrVar::Unknown => {}
    //         // };

    //         // if rhs_is_mut_ref || pat_ident.mutability.is_some() || fn_call_mut_ref {
    //         //     global_data.scopes.last_mut().unwrap().0.push(scoped_var);
    //         // }
    //         global_data
    //             .scopes
    //             .last_mut()
    //             .unwrap()
    //             .variables
    //             .push(scoped_var);
    //     }
    //     _ => {}
    // }
}

pub fn handle_stmt(
    stmt: &Stmt,
    global_data: &mut GlobalData,
    current_module_path: &[String],
) -> Vec<(JsStmt, RustType)> {
    match stmt {
        Stmt::Expr(expr, closing_semi) => {
            let (mut js_expr, type_) = handle_expr(expr, global_data, current_module_path);
            // copying etc should be handled in handle_expr, not here?
            // if should_copy_expr_unary(expr, global_data) {
            //     js_expr = JsExpr::MethodCall(Box::new(js_expr), "copy".to_string(), Vec::new());
            // }

            // TODO this is the only case where the returned RustType is not RustType::Unit, specifically where there is no semi, but surely this shouldn't be considered a stmt and then we wouldn't have to worry about returning the RustType. I guess it is feasible that while me might have a stmt in *Rust*, what we want to transpile it to is merely an expression. should add a todo here to find what cases it is being used for.
            vec![(
                JsStmt::Expr(js_expr, closing_semi.is_some()),
                if closing_semi.is_some() {
                    RustType::Unit
                } else {
                    type_
                },
            )]
        }
        Stmt::Local(local) => vec![(
            handle_local(local, global_data, current_module_path),
            RustType::Unit,
        )],
        Stmt::Item(item) => match item {
            // TODO this should all be handled by `fn handle_item()`??? Yes, but need to remove `at_module_top_level: bool,` args form `handle_` fns, and better to do that when the codebase is more settled and we have more tests to avoid introducing bugs from managing `at_module_top_level` on GlobalData.
            Item::Const(item_const) => vec![(
                handle_item_const(item_const, false, global_data, current_module_path),
                RustType::Unit,
            )],
            Item::Enum(item_enum) => vec![(
                handle_item_enum(item_enum.clone(), false, global_data, current_module_path),
                RustType::Unit,
            )],
            Item::ExternCrate(_) => todo!(),
            Item::Fn(item_fn) => vec![(
                handle_item_fn(item_fn, false, global_data, current_module_path),
                RustType::Unit,
            )],
            Item::ForeignMod(_) => todo!(),
            Item::Impl(item_impl) => {
                // handle_item_impl(item_impl, false, global_data, current_module_path);
                // (JsStmt::Expr(JsExpr::Vanish, false), RustType::Unit)
                // (
                //     handle_item_impl(item_impl, false, global_data, current_module_path),
                //     RustType::Unit,
                // )
                handle_item_impl(item_impl, false, global_data, current_module_path)
                    .into_iter()
                    .map(|stmt| (stmt, RustType::Unit))
                    .collect()
            }
            Item::Macro(_) => todo!(),
            Item::Mod(_) => todo!(),
            Item::Static(_) => todo!(),
            // Item::Struct(_) => JsStmt::Expr(JsExpr::Vanish, false),
            Item::Struct(item_struct) => vec![(
                handle_item_struct(item_struct, false, global_data, current_module_path),
                RustType::Unit,
            )],
            Item::Trait(item_trait) => {
                handle_item_trait(item_trait, false, global_data, current_module_path);
                vec![(JsStmt::Expr(JsExpr::Vanish, false), RustType::Unit)]
            }
            Item::TraitAlias(_) => todo!(),
            Item::Type(_) => todo!(),
            Item::Union(_) => todo!(),
            // Item::Use(item_use) => handle_item_use(item_use),
            // TODO surely need to handle scoped use statements as they could shadow other item idents?
            Item::Use(item_use) => {
                let scope = global_data.scopes.last_mut().unwrap();
                handle_item_use(item_use, ItemUseModuleOrScope::Scope(scope));
                vec![(JsStmt::Expr(JsExpr::Vanish, false), RustType::Unit)]
            }
            Item::Verbatim(_) => todo!(),
            _ => todo!(),
        },
        Stmt::Macro(stmt_macro) => vec![(
            JsStmt::Expr(
                handle_expr_and_stmt_macro(&stmt_macro.mac, global_data, current_module_path).0,
                stmt_macro.semi_token.is_some(),
            ),
            RustType::Unit,
        )],
    }
}
