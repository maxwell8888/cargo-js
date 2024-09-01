use syn::Pat;

use super::{
    handle_pat,
    handle_syn_expr::{
        handle_expr, handle_expr_and_stmt_macro, handle_expr_block, handle_expr_match,
        handle_expr_path,
    },
    handle_syn_item::{
        handle_item_const, handle_item_enum, handle_item_fn, handle_item_impl, handle_item_struct,
        handle_item_trait,
    },
    parse_fn_input_or_field, PartialRustType,
};

use super::definition_data::{RustType2, ScopedVar};
use crate::{
    js_ast::{Ident, JsExpr, JsIf, JsLocal, JsStmt, LocalName, LocalType, PathIdent},
    make_item_definitions::{ExprRef, ItemRef, LocalRef, RustExprPath, StmtsRef},
    update_item_definitions::{ItemDef, ItemDefRc, StructEnumUniqueInfo2},
    GlobalData,
};

fn handle_local(
    local: &LocalRef,
    global_data: &mut GlobalData,
    current_module_path: &[String],
) -> JsStmt {
    // let span = debug_span!("handle_local", lhs = ?quote! { #local }.to_string());
    // let _guard = span.enter();

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
    let (mut rhs_expr, rhs_type, rhs_is_mut_var) = match &*local_init.expr {
        // TODO Why are we calling handle_expr_path separately here? To know if we have a mut var?
        ExprRef::Path(expr_path) => {
            let (expr, partial) = handle_expr_path(expr_path, global_data, current_module_path);
            match partial {
                PartialRustType::StructIdent(type_params, struct_enum_def) => {
                    // TODO feels wrong to be extracting data from a parsed JsExpr?
                    let path_ident = match expr {
                        JsExpr::Path(path_ident) => path_ident,
                        _ => todo!(),
                    };
                    (
                        JsExpr::New(path_ident, vec![]),
                        RustType2::StructOrEnum(type_params, struct_enum_def),
                        false,
                    )
                }
                PartialRustType::EnumVariantIdent(_, _, _) => todo!(),
                PartialRustType::RustType(rust_type, is_mut_var, _is_var) => {
                    (expr, rust_type, is_mut_var)
                }
            }
        }
        _ => {
            let (expr, rust_type) = handle_expr(&local_init.expr, global_data, current_module_path);
            (expr, rust_type, false)
        }
    };

    // NOTE This doesn't distinguish between say a var that has type MutRef, and a literal like `&mut 5`.
    let _rhs_is_mut_ref = matches!(rhs_type, RustType2::MutRef(_));

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
                    Pat::Type(pat_type) => var_name_matches(&pat_type.pat, var_name),
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
    let mut handle_should_add_copy_expr_path = |expr_path: &RustExprPath| {
        if expr_path.path.segments.len() == 1 {
            let scopes_clone = global_data.scopes.clone();
            let var = scopes_clone.iter().rev().find_map(|s| {
                s.variables
                    .iter()
                    .find(|v| expr_path.path.segments.first().unwrap().ident == v.name)
            });
            if let Some(var) = var {
                // TODO impl PartialEq for RustType2
                // assert!(var.type_ == rhs_type);

                // Get var item def so we can check if var is a copy struct or an array
                fn handle_type(
                    // NOTE We need to specify the RustType separately to the ScopedVar to allow passing in the inner type of a RustType::MutRef
                    rust_type: &RustType2,
                    global_data: &mut GlobalData,
                    var: &ScopedVar,
                    lhs: &LocalName,
                    mut_ref_taken: bool,
                ) -> bool {
                    match rust_type {
                        RustType2::Unit => todo!(),
                        RustType2::Never => todo!(),
                        RustType2::ImplTrait(_) => todo!(),
                        RustType2::TypeParam(_) => todo!(),
                        RustType2::I32 => false,
                        RustType2::F32 => todo!(),
                        RustType2::Bool => todo!(),
                        RustType2::String => todo!(),
                        RustType2::Option(_) => todo!(),
                        RustType2::Result(_, _) => todo!(),
                        RustType2::StructOrEnum(_type_params, item_def) => {
                            item_def.is_copy && var.mut_ && !mut_ref_taken
                        }
                        RustType2::Vec(_) => todo!(),
                        RustType2::Array(element_type) => {
                            // (although this won't be necessary if the previous value is not used after the move/copy, but this would be hard to determine so need to just always add copy)
                            match lhs {
                                LocalName::Single(_) => true,
                                LocalName::DestructureObject(_) => todo!(),
                                LocalName::DestructureArray(_) => {
                                    // If element_type is `Copy` struct/enum then should add `.copy()` to rhs array
                                    match &**element_type {
                                        RustType2::StructOrEnum(_type_params, item_def) => {
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
                        RustType2::Tuple(_) => todo!(),
                        RustType2::UserType(_, _) => todo!(),
                        RustType2::MutRef(inner) => {
                            // A &mut T is always `Copy` copied (not the T, the reference) so don't need to `.copy()` it, unless it is dereferenced - *or destructured*
                            handle_type(inner, global_data, var, lhs, true)
                        }
                        RustType2::Ref(_) => todo!(),
                        RustType2::Fn(_, _, _) => todo!(),
                        RustType2::Closure(_, _) => todo!(),
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
        ExprRef::Async(_) => todo!(),
        ExprRef::Await(_) => todo!(),
        // TODO not sure if this is correct, need to add some tests for objects that need `.copy()`ing and are returned from blocks
        ExprRef::Block(_) => false,
        ExprRef::Cast(_) => todo!(),
        ExprRef::Const(_) => todo!(),
        ExprRef::Continue(_) => todo!(),
        ExprRef::Field(_) => todo!(),
        ExprRef::ForLoop(_) => todo!(),
        ExprRef::Group(_) => todo!(),
        ExprRef::Index(_) => todo!(),
        ExprRef::Infer(_) => todo!(),
        ExprRef::Let(_) => todo!(),
        ExprRef::Loop(_) => todo!(),
        ExprRef::Paren(_) => todo!(),
        ExprRef::Path(expr_path) => handle_should_add_copy_expr_path(expr_path),
        ExprRef::Range(_) => todo!(),
        ExprRef::Reference(expr_reference) => {
            if expr_reference.mutability {
                match &expr_reference.expr {
                    // Expr::Path(expr_path) => {
                    //     handle_should_add_copy_expr_path(expr_path)
                    // }
                    _ => false,
                }
            } else {
                false
            }
        }
        ExprRef::Repeat(_) => todo!(),
        ExprRef::Return(_) => todo!(),
        ExprRef::Try(_) => todo!(),
        ExprRef::TryBlock(_) => todo!(),
        ExprRef::Unary(_) => false,
        // TODO handle inner expr properly
        ExprRef::Unsafe(_) => false,
        ExprRef::Verbatim(_) => todo!(),
        ExprRef::While(_) => todo!(),
        ExprRef::Yield(_) => todo!(),
        _ => false,
    };
    // TODO We have two `rhs_should_add_copy`s because for some cases it is easier to check the syn expr, in some cases it is easier/necessary to look at the parsed RustType. Ideally we would store sufficient info on the RustType to combine these??
    // let rhs_should_add_copy2 = match &rhs_type {
    //     ...
    // };
    // if rhs_should_add_copy || rhs_should_add_copy2 {
    if rhs_should_add_copy {
        rhs_expr = JsExpr::MethodCall(Box::new(rhs_expr), Ident::Str("copy"), Vec::new());
    }

    if !lhs_is_mut && rhs_is_mut_var && rhs_type.is_js_primative() {
        rhs_expr = JsExpr::Field(Box::new(rhs_expr), Ident::Str("inner"));
    }

    if lhs_is_mut && rhs_type.is_js_primative() {
        rhs_expr = match rhs_type {
            RustType2::NotAllowed => todo!(),
            RustType2::Unknown => todo!(),
            RustType2::Todo => todo!(),
            RustType2::Unit => todo!(),
            RustType2::Never => todo!(),
            RustType2::ImplTrait(_) => todo!(),
            RustType2::TypeParam(_) => todo!(),
            RustType2::I32 => {
                global_data.rust_prelude_types.rust_integer = true;
                if rhs_is_mut_var {
                    JsExpr::New(
                        "RustInteger".into(),
                        vec![JsExpr::Field(Box::new(rhs_expr), Ident::Str("inner"))],
                    )
                } else {
                    JsExpr::New("RustInteger".into(), vec![rhs_expr])
                }
            }
            RustType2::F32 => todo!(),
            RustType2::Bool => todo!(),
            RustType2::String => {
                global_data.rust_prelude_types.rust_string = true;
                JsExpr::New("RustString".into(), vec![rhs_expr])
            }
            RustType2::Option(_) => todo!(),
            RustType2::Result(_, _) => todo!(),
            RustType2::StructOrEnum(_, _) => todo!(),
            RustType2::Vec(_) => todo!(),
            RustType2::Array(_) => todo!(),
            RustType2::Tuple(_) => todo!(),
            RustType2::UserType(_, _) => todo!(),
            RustType2::MutRef(_) => rhs_expr,
            RustType2::Ref(_) => todo!(),
            RustType2::Fn(_, _, _) => todo!(),
            RustType2::Closure(_, _) => rhs_expr,
            RustType2::FnVanish => todo!(),
            RustType2::Box(_) => todo!(),
            RustType2::Self_ => todo!(),
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
                            LocalName::Single(name) => JsExpr::Path(PathIdent::Single(name)),
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
    stmt: &StmtsRef,
    global_data: &mut GlobalData,
    current_module_path: &[String],
) -> Vec<(JsStmt, RustType2)> {
    // TODO this should all be handled by `fn handle_item()`??? Yes, but need to remove `at_module_top_level: bool,` args form `handle_` fns, and better to do that when the codebase is more settled and we have more tests to avoid introducing bugs from managing `at_module_top_level` on GlobalData.
    let item_defs = global_data.item_defs.clone();
    match stmt {
        StmtsRef::Local(local) => {
            vec![(
                handle_local(local, global_data, current_module_path),
                RustType2::Unit,
            )]
        }
        StmtsRef::Item(item_ref) => {
            match item_ref {
                ItemRef::StructOrEnum(index) => {
                    let item = &item_defs.clone()[*index];
                    match item {
                        ItemDefRc::StructEnum(actual) => match &actual.struct_or_enum_info {
                            StructEnumUniqueInfo2::Struct(_struct_def) => {
                                vec![(
                                    handle_item_struct(
                                        *index,
                                        false,
                                        global_data,
                                        current_module_path,
                                    ),
                                    RustType2::Unit,
                                )]
                            }
                            StructEnumUniqueInfo2::Enum(_enum_def) => {
                                vec![(
                                    handle_item_enum(
                                        *index,
                                        false,
                                        global_data,
                                        current_module_path,
                                    ),
                                    RustType2::Unit,
                                )]
                            }
                        },
                        _ => todo!(),
                    }
                }
                ItemRef::Fn(index) => {
                    vec![(
                        handle_item_fn(*index, false, global_data, current_module_path),
                        RustType2::Unit,
                    )]
                }
                ItemRef::Const(index) => {
                    vec![(
                        handle_item_const(*index, false, global_data, current_module_path),
                        RustType2::Unit,
                    )]
                }
                ItemRef::Trait(index) => {
                    vec![(
                        handle_item_trait(*index, true, global_data, current_module_path),
                        RustType2::Unit,
                    )]
                }
                ItemRef::Impl(index) => {
                    // TODO maybe it would be better for handle_item_impl (and similar fns) to return a JsClass and then we wrap it into a stmt here?
                    handle_item_impl(*index, false, global_data, current_module_path)
                        .into_iter()
                        .map(|stmt| (stmt, RustType2::Unit))
                        .collect()
                }
                ItemRef::Mod(_rust_mod) => todo!(),
                ItemRef::Use(rust_use) => {
                    // TODO surely need to handle scoped use statements as they could shadow other item idents?
                    // handle_item_use(item_use, ItemUseModuleOrScope::Scope(scope));
                    // vec![(JsStmt::Expr(JsExpr::Vanish, false), RustType2::Unit)]
                    let scope = global_data.scopes.last_mut().unwrap();
                    scope.use_mappings.extend(rust_use.use_mappings.clone());

                    vec![(JsStmt::Expr(JsExpr::Vanish, false), RustType2::Unit)]
                }
                ItemRef::Macro => todo!(),
            }
        }
        StmtsRef::Expr(expr, closing_semi) => {
            let (js_expr, type_) = handle_expr(expr, global_data, current_module_path);
            // copying etc should be handled in handle_expr, not here?
            // if should_copy_expr_unary(expr, global_data) {
            //     js_expr = JsExpr::MethodCall(Box::new(js_expr), "copy".to_string(), Vec::new());
            // }

            // TODO this is the only case where the returned RustType is not RustType::Unit, specifically where there is no semi, but surely this shouldn't be considered a stmt and then we wouldn't have to worry about returning the RustType. I guess it is feasible that while me might have a stmt in *Rust*, what we want to transpile it to is merely an expression. should add a todo here to find what cases it is being used for.
            vec![(
                JsStmt::Expr(js_expr, *closing_semi),
                if *closing_semi {
                    RustType2::Unit
                } else {
                    type_
                },
            )]
        }
        StmtsRef::Macro(stmt_macro) => {
            vec![(
                JsStmt::Expr(
                    handle_expr_and_stmt_macro(stmt_macro, global_data, current_module_path).0,
                    stmt_macro.semi_token,
                ),
                RustType2::Unit,
            )]
        }
    }
}

pub fn parse_fn_body_stmts(
    is_arrow_fn: bool,
    // TODO rename this to be clearer it means not &mut rather than simply &
    returns_non_mut_ref_val: bool,
    // `return` can only be used in fns and closures so need this to prevent them being added to blocks
    allow_return: bool,
    stmts: &[StmtsRef],
    global_data: &mut GlobalData,
    current_module: &[String],
) -> (Vec<JsStmt>, RustType2) {
    // let mut return_type = RustType::Todo;
    let mut js_stmts = Vec::new();

    // let (js_stmts, types): (Vec<_>, Vec<_>) = stmts
    //     .iter()
    //     .enumerate()
    //     .map(|(i, stmt)| )
    //     .unzip();

    // It is important to be able to generate no body arrow fns like `(x) => x + 1` eg for `.map()` etc but we need to be able to determine whether the resultant expression is suitable to fit in or should be within braces and thus require a return statement, which is not straightforward. Eg a call might be a single line depending on how many args/length of idents, a macro might be depending on what it expands/transpiles to, etc. Really we need to parse it, format it, then check whether it is a single line. We take a simplified approach here.
    let is_single_expr_return = if stmts.len() == 1 {
        match stmts.first().unwrap() {
            StmtsRef::Local(_) => false,
            StmtsRef::Item(_) => false,
            StmtsRef::Expr(expr, _) => match expr {
                ExprRef::Array(_) => true,
                ExprRef::Assign(_) => true,
                ExprRef::Async(_) => todo!(),
                ExprRef::Await(_) => true,
                ExprRef::Binary(_) => true,
                ExprRef::Call(_) => true,
                ExprRef::Cast(_) => true,
                ExprRef::Field(_) => true,
                // TODO should be true for if expressions that transpile to a (short) ternary
                ExprRef::If(_) => false,
                ExprRef::Index(_) => true,
                ExprRef::Lit(_) => true,
                ExprRef::Macro(_) => true,
                ExprRef::MethodCall(_) => true,
                ExprRef::Paren(_) => true,
                ExprRef::Path(_) => true,
                ExprRef::Range(_) => todo!(),
                ExprRef::Reference(_) => true,
                ExprRef::Repeat(_) => true,
                ExprRef::Struct(_) => true,
                ExprRef::Tuple(_) => true,
                ExprRef::Unary(_) => true,
                ExprRef::Unsafe(_) => todo!(),
                ExprRef::Verbatim(_) => todo!(),
                _ => false,
            },
            StmtsRef::Macro(_) => true,
        }
    } else {
        false
    };

    // TODO loads of duplication here and needs documenting why each special case is requried
    let mut return_type = None;
    for (i, stmt) in stmts.iter().enumerate() {
        // Manually set assignment var name for if expressions that are a return stmt
        if i == stmts.len() - 1 {
            match stmt {
                StmtsRef::Expr(expr, semi) => match expr {
                    // TODO how is this different to the normal Expr::If handling??? Is this unnecessary duplication?
                    ExprRef::If(expr_if) => {
                        if *semi {
                            let stmts = handle_stmt(stmt, global_data, current_module);
                            return_type = Some(stmts.last().unwrap().1.clone());
                            js_stmts.extend(stmts.into_iter().map(|(stmt, _type_)| stmt));
                        } else {
                            // TODO should be using same code to parse Expr::If as elsewhere in code
                            let (condition, type_) =
                                handle_expr(&expr_if.cond, global_data, current_module);
                            let condition = Box::new(condition);

                            let fail = expr_if.else_branch.as_ref().map(|expr| {
                                //
                                match &**expr {
                                    ExprRef::Block(expr_block) => {
                                        // Box::new(handle_expr(&*expr, global_data, current_module).0)
                                        // println!("{}", quote! { #expr_block });
                                        Box::new(
                                            handle_expr_block(
                                                expr_block,
                                                global_data,
                                                current_module,
                                                false,
                                                false,
                                            )
                                            .0,
                                        )
                                    }
                                    ExprRef::If(_) => {
                                        Box::new(handle_expr(expr, global_data, current_module).0)
                                    }
                                    _ => panic!(),
                                }
                            });
                            let stmt = JsStmt::Expr(
                                JsExpr::If(JsIf {
                                    assignment: Some(LocalName::Single(Ident::Str(
                                        "ifTempAssignment",
                                    ))),
                                    declare_var: true,
                                    condition,
                                    succeed: expr_if
                                        .then_branch
                                        .iter()
                                        .flat_map(|stmt| {
                                            handle_stmt(stmt, global_data, current_module)
                                                .into_iter()
                                                .map(|(stmt, _type_)| stmt)
                                        })
                                        .collect(),
                                    fail,
                                }),
                                false,
                            );
                            js_stmts.push(stmt);
                            return_type = Some(type_);
                        }
                    }
                    ExprRef::Match(expr_match) => {
                        if *semi {
                            let stmts = handle_stmt(stmt, global_data, current_module);
                            return_type = Some(stmts.last().unwrap().1.clone());
                            js_stmts.extend(stmts.into_iter().map(|(stmt, _type_)| stmt));
                        } else {
                            let (if_expr, type_) =
                                handle_expr_match(expr_match, true, global_data, current_module);
                            js_stmts.push(JsStmt::Expr(if_expr, true));
                            js_stmts.push(JsStmt::Expr(
                                JsExpr::Return(Box::new(JsExpr::Path(PathIdent::Single(
                                    Ident::Str("ifTempAssignment"),
                                )))),
                                true,
                            ));
                            return_type = Some(type_);
                        }
                    }
                    ExprRef::Path(expr_path)
                        if returns_non_mut_ref_val
                            && expr_path.path.segments.len() == 1
                            && !semi =>
                    {
                        // NOTE a len=1 path could also be a const or a fn
                        let var_name = expr_path.path.segments.first().unwrap().ident.to_string();
                        let var_info = global_data
                            .scopes
                            .iter()
                            .rev()
                            .find_map(|s| s.variables.iter().rev().find(|v| v.name == var_name))
                            .unwrap();
                        let mut js_var = JsExpr::Path(PathIdent::Single(Ident::String(var_name)));
                        if var_info.mut_ {
                            js_var = JsExpr::Field(Box::new(js_var), Ident::Str("inner"))
                        }
                        let stmt = JsStmt::Expr(JsExpr::Return(Box::new(js_var)), true);
                        // Lookup path to get return type
                        // JsStmt::Expr(JsExpr::Return(Box::new(js_var)), true)
                        js_stmts.push(stmt);
                        return_type = Some(var_info.type_.clone());
                    }
                    // Expr::Unary(expr_unary) if returns_non_mut_ref_val && semi.is_none() => {
                    //     // if equivalent to JS primitive deref of mut/&mut number, string, or boolean, then call inner, else call copy (note we are only handling paths at the mo as we can find the types for them)
                    //     // TODO this logic and other stuff in this fn is duplicating stuff that should/does already exist in handle_expr
                    //     // The problem is we need to know `returns_non_mut_ref_val`?

                    //     let (expr, type_) =
                    //         handle_expr(&*expr_unary.expr, global_data, current_module);
                    //     // return_type = type_;

                    //     js_stmts.push(JsStmt::Expr(expr, true));
                    //     return_type = Some(type_);
                    //     // (JsStmt::Expr(expr, false), type_)
                    // }
                    _other => {
                        // println!("{}", quote! { #other });
                        if *semi {
                            let stmts = handle_stmt(stmt, global_data, current_module);
                            return_type = Some(stmts.last().unwrap().1.clone());
                            js_stmts.extend(stmts.into_iter().map(|(stmt, _type_)| stmt));
                        } else {
                            // dbg!("print expr");
                            // println!("{}", quote! { #expr });
                            let (mut js_expr, type_, is_mut) = match expr {
                                ExprRef::Path(expr_path) => {
                                    let (expr, partial) =
                                        handle_expr_path(expr_path, global_data, current_module);
                                    match partial {
                                        PartialRustType::StructIdent(_, _) => todo!(),
                                        PartialRustType::EnumVariantIdent(_, _, _) => {
                                            todo!()
                                        }
                                        PartialRustType::RustType(type_, is_mut, _is_var) => {
                                            (expr, type_, is_mut)
                                        }
                                    }
                                }
                                _ => {
                                    let (expr, type_) =
                                        handle_expr(expr, global_data, current_module);
                                    (expr, type_, false)
                                }
                            };

                            // Is the thing being returned a JS primative mut var or &mut (ie has a RustInteger wrapper)? in which case we need to get the inner value if `returns_non_mut_ref_val` is true

                            // TODO leave false for now until I clean up/refactor this code since this `is_js_primative_mut_var` should get caught be the Expr::Path branch
                            let is_js_primative_mut_var = is_mut && type_.is_js_primative();
                            let is_js_primative_mut_ref = type_
                                .is_mut_ref_of_js_primative(&global_data.impl_block_target_type);

                            // dbg!(returns_non_mut_ref_val);
                            // dbg!(is_js_primative_mut_var);
                            // dbg!(is_js_primative_mut_ref);
                            if returns_non_mut_ref_val
                                && (is_js_primative_mut_var || is_js_primative_mut_ref)
                            {
                                js_expr = JsExpr::Field(Box::new(js_expr), Ident::Str("inner"));
                            }

                            let return_expr =
                                if (is_arrow_fn && is_single_expr_return) || !allow_return {
                                    JsStmt::Expr(js_expr, true)
                                } else {
                                    JsStmt::Expr(JsExpr::Return(Box::new(js_expr)), true)
                                };
                            js_stmts.push(return_expr);
                            return_type = Some(type_);
                        }
                    }
                },
                _ => {
                    let stmts = handle_stmt(stmt, global_data, current_module);
                    return_type = Some(stmts.last().unwrap().1.clone());
                    js_stmts.extend(stmts.into_iter().map(|(stmt, _type_)| stmt));
                }
            }
        } else {
            let stmts = handle_stmt(stmt, global_data, current_module);
            return_type = Some(stmts.last().unwrap().1.clone());
            js_stmts.extend(stmts.into_iter().map(|(stmt, _type_)| stmt));
        }
    }

    if stmts.is_empty() {
        (Vec::new(), RustType2::Unit)
    } else {
        (js_stmts, return_type.unwrap())
    }
}
