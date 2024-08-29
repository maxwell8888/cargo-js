use std::rc::Rc;

use quote::quote;
use syn::{BinOp, Expr, GenericArgument, Index, Lit, Member, Pat, UnOp};
use tracing::{debug, debug_span, warn};

use super::{
    definition_data::{GlobalData, RustTypeParam2},
    handle_syn_stmt::{handle_stmt, parse_fn_body_stmts},
    parse_fn_input_or_field,
};

use super::{
    definition_data::{RustType2, RustTypeParamValue2, ScopedVar},
    handle_pat, PartialRustType, RustPathSegment2,
};

use crate::{
    handle_syn::{definition_data::resolve_path, GlobalDataScope},
    js_ast::{
        DestructureObject, DestructureValue, Ident, JsExpr, JsFn, JsIf, JsLocal, JsOp, JsStmt,
        LocalName, LocalType, PathIdent,
    },
    make_item_definitions::{
        expr_to_expr_ref, stmt_to_stmts_ref, ExprRef, ItemRef, RustExprAssign, RustExprBlock,
        RustExprCall, RustExprClosure, RustExprMatch, RustExprMethodCall, RustExprOrStmtMacro,
        RustExprPath, StmtsRef,
    },
    update_item_definitions::{
        EnumVariantInputsInfo, FnDef, ItemDef, ItemDefRc, RustImplItemItemNoJs, RustImplItemNoJs,
        RustTypeParam, RustTypeParamValue, StructEnumDef, StructEnumUniqueInfo2, StructFieldInfo,
    },
    RustType, RUST_PRELUDE_MODULE_PATH,
};

fn handle_expr_assign(
    expr_assign: &RustExprAssign,
    global_data: &mut GlobalData,
    current_module: &[String],
) -> JsExpr {
    let (mut lhs_expr, lhs_rust_type, lhs_is_mut_var) = match &*expr_assign.left {
        ExprRef::Path(expr_path) => {
            let (expr, partial) = handle_expr_path(expr_path, global_data, current_module);
            match partial {
                PartialRustType::StructIdent(_, _) => todo!(),
                PartialRustType::EnumVariantIdent(_, _, _) => todo!(),
                PartialRustType::RustType(rust_type, is_mut_var, _is_var) => {
                    (expr, rust_type, is_mut_var)
                }
            }
        }
        _ => {
            let (expr, rust_type) = handle_expr(&expr_assign.left, global_data, current_module);
            (expr, rust_type, false)
        }
    };

    // If we are assigning, lhs must be mut var (or a derefferenced &mut)
    // assert!(lhs_is_mut_var);

    let (mut rhs_expr, rhs_rust_type, rhs_is_mut_var) = match &*expr_assign.right {
        ExprRef::Path(expr_path) => {
            let (expr, partial) = handle_expr_path(expr_path, global_data, current_module);
            match partial {
                PartialRustType::StructIdent(_, _) => todo!(),
                PartialRustType::EnumVariantIdent(_, _, _) => todo!(),
                PartialRustType::RustType(rust_type, is_mut_var, _is_var) => {
                    (expr, rust_type, is_mut_var)
                }
            }
        }
        _ => {
            let (expr, rust_type) = handle_expr(&expr_assign.right, global_data, current_module);
            (expr, rust_type, false)
        }
    };

    let lhs_is_mut_ref = matches!(lhs_rust_type, RustType2::MutRef(_));
    let rhs_is_mut_ref = matches!(rhs_rust_type, RustType2::MutRef(_));

    #[allow(clippy::nonminimal_bool)]
    if lhs_is_mut_var && ((!rhs_is_mut_var && !rhs_is_mut_ref) || rhs_is_mut_var) {
        lhs_expr = JsExpr::Field(Box::new(lhs_expr), Ident::Str("inner"));
    }
    if lhs_is_mut_var && rhs_is_mut_var {
        rhs_expr = JsExpr::Field(Box::new(rhs_expr), Ident::Str("inner"));
    }

    // If `var mut num = 1;` or `var num = &mut 1` or `var mut num = &mut 1` then wrap num literal in RustInteger or RustFLoat
    // what if we have a fn returning an immutable integer which is then getting made mut or &mut here? or a field or if expression or parens or block or if let or match or method call or ... . We just check for each of those constructs, and analyse them to determine the return type? Yes but this is way easier said than done so leave it for now but start record var type info as a first step towards being able to do this analysis.
    // determining types
    // easy: fn calls, method calls, fields,
    // hard: if expression, parens, block, if let, match, method call

    // Add .copy() if rhs is a mut var
    // and rhs is `Copy`

    // If lhs is mut var then we overwrite the value ie `foo = bar`
    // If lhs is deref of &mut then update the "inner" value (need to think how this applies to structs etc) ie `foo.inner = bar`
    // First assert that we can never have an assign where the lhs is not a mut var or deref of a &mut

    // Get lhs var
    // NOTE we should only look for vars in "transparent" scopes like blocks, once we reach an "opaque" block like a fn scope, we can stop looking. Also need to look for global/module level, and scoped, consts and fns
    // lhs must be either an ident or a deref ident
    // NOTE I don't think we need to ignore derefs of not-mut refs because it is not possible to have a non-mut ref on the lhs of an assignment
    // NOTE need to know RustType of each derefed expr to know whether to add a `.inner` or not (ie only added to JS copy only primatives)
    fn _get_name_and_deref(expr: &Expr) -> (String, bool) {
        match expr {
            Expr::Array(_) => todo!(),
            Expr::Field(_) => todo!(),
            Expr::Macro(_) => todo!(),
            Expr::Paren(_) => todo!(),
            Expr::Path(expr_path) => {
                assert!(expr_path.path.segments.len() == 1);
                (
                    expr_path.path.segments.first().unwrap().ident.to_string(),
                    false,
                )
            }
            Expr::Reference(_) => todo!(),
            Expr::Struct(_) => todo!(),
            Expr::Tuple(_) => todo!(),
            Expr::Unary(expr_unary) => match expr_unary.op {
                UnOp::Deref(_) => (_get_name_and_deref(&expr_unary.expr).0, true),
                UnOp::Not(_) => todo!(),
                UnOp::Neg(_) => todo!(),
                _ => todo!(),
            },
            Expr::Unsafe(_) => todo!(),
            Expr::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }
    // let (ident_name, is_deref) = get_name_and_deref(&*expr_assign.left);
    // // NOTE that whilst we can assign to a var *holding* a fn eg:
    // {
    //     fn cool() -> i32 {
    //         4
    //     }
    //     fn bean() -> i32 {
    //         7
    //     }
    //     let one = &mut (cool as fn() -> i32);
    //     *one = bean as fn() -> i32;
    //     assert!(one() == 7);
    // }
    // // we can't actuall assign directly to a fn like `cool = bean as fn() -> i32` so any fn idents appearing on the lhs is an error. The rhs can be a fn or const though.
    // let scoped_var = global_data.scopes.iter().rev().find_map(|s| {
    //     // TODO would be nice to assert here that the lhs cannot be a fn or const (because fns and const cannot be directly assigned to), but we don't keep track of the order of vars/fns/etc added to the scope so can't know for sure! Would need to store vars/fns/etc in the same Vec
    //     // if s.fns.iter().any(|func| (func.ident == ident_name)) {
    //     //     panic!();
    //     // }

    //     s.variables
    //         .iter()
    //         .find(|variable| variable.name == ident_name)
    // });
    // let is_lhs_mut = scoped_var.unwrap().mut_;

    if !lhs_is_mut_ref && rhs_is_mut_ref {
        rhs_expr = JsExpr::Field(Box::new(rhs_expr), Ident::Str("inner"));
    }
    JsExpr::Assignment(Box::new(lhs_expr), Box::new(rhs_expr))

    // TODO
    // let rhs_is_deref_mut_ref = ...

    // let lhs_is_mut = match &local.pat {
    //     Pat::Ident(pat_ident) => pat_ident.mutability.is_some(),
    //     _ => false,
    // };
    // let rhs_is_var = match &*local.init.as_ref().unwrap().expr {
    //     Expr::Path(expr_path) => expr_path.path.segments.len() == 1,
    //     _ => false,
    // };

    // Get var info
    // let type_or_var = get_type_from_expr(&rhs_expr, global_data, current_module);

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

    // if rhs_is_deref && rhs_is_found_var.map(|v| v.is_mut_ref()).unwrap_or(false) {
    //     {
    //         let num = &mut 5; // or let mut num = &mut 5;
    //         let mut copy = 5;
    //         copy = *num;
    //         // TODO handle:
    //         let five = &mut &mut 5;
    //         let mut copy = &mut 5;
    //         copy = *five;
    //     }
    //     rhs = JsExpr::MethodCall(Box::new(rhs), "inner".to_string(), vec![]);
    // }
    // // copy rhs if is mut and is a variable, which is being assigned
    // else if rhs_is_found_var
    //     .map(|v| v.mut_ && !v.is_mut_ref())
    //     .unwrap_or(false)
    // {
    //     // NOTE if !v.mut_ then we just have `var num = 5; copy = num;` which behaves as expected and doesn't need any wrapper
    //     {
    //         let mut num = 5;
    //         let mut copy = 5;
    //         copy = num;
    //     }
    //     rhs = JsExpr::MethodCall(Box::new(rhs), "inner".to_string(), vec![]);
    // } else if rhs_is_deref && rhs_is_fn_call {
    //     // TODO for now just assume that any call being dereferenced returns a &mut and should be `.copy()`d. I don't think this will cause any incorrect behavior, only unnecessary copying, eg where the return is `&i32` not `&mut i32`
    //     // copy = *some_fn();
    //     rhs = JsExpr::MethodCall(Box::new(rhs), "inner".to_string(), vec![]);
    // } else if rhs_takes_mut_ref {
    //     {
    //         let mut num = &mut 0;
    //         let mut orig = 5;

    //         num = &mut 5;
    //         // or
    //         num = &mut orig;
    //     }
    //     // TODO handle rhs not being a var or literal, eg fn call etc
    //     if let Some(rhs_is_found_var) = rhs_is_found_var {
    //         match rhs_is_found_var.type_ {
    //             RustType::Todo => {}
    //             RustType::Unit => {}
    //             RustType::I32 => {
    //                 global_data.rust_prelude_types.integer = true;
    //                 rhs = JsExpr::New(vec!["RustInteger".to_string()], vec![rhs]);
    //             }
    //             RustType::F32 => todo!(),
    //             RustType::Bool => {
    //                 global_data.rust_prelude_types.bool = true;
    //                 rhs = JsExpr::New(vec!["RustBool".to_string()], vec![rhs])
    //             }
    //             RustType::String => {
    //                 global_data.rust_prelude_types.string = true;
    //                 rhs = JsExpr::New(vec!["RustString".to_string()], vec![rhs]);
    //             }
    //             RustType::StructOrEnum(_, _, _) => {}
    //             // RustType::Enum(_, _) => {}
    //             RustType::NotAllowed => {}
    //             RustType::Unknown => {}
    //             RustType::Never => {}
    //             RustType::Vec(_) => {}
    //             RustType::Array(_) => {}
    //             RustType::Tuple(_) => {}
    //             RustType::MutRef(_) => {}
    //             RustType::Fn(_, _, _, _) => {}
    //             RustType::Option(_) => {}
    //             RustType::Result(_) => {}
    //             RustType::TypeParam(_) => {}
    //             RustType::ImplTrait(_) => {}
    //             RustType::ParentItem => {}
    //             RustType::UserType(_, _) => {}
    //             RustType::Ref(_) => {}
    //         }
    //     } else {
    //         match &rhs_expr {
    //             Expr::Lit(expr_lit) => match &expr_lit.lit {
    //                 Lit::Str(lit_str) => {
    //                     global_data.rust_prelude_types.string = true;
    //                     rhs = JsExpr::New(
    //                         vec!["RustString".to_string()],
    //                         vec![JsExpr::LitStr(lit_str.value())],
    //                     );
    //                 }
    //                 Lit::ByteStr(_) => {}
    //                 Lit::Byte(_) => {}
    //                 Lit::Char(_) => {}
    //                 Lit::Int(lit_int) => {
    //                     global_data.rust_prelude_types.integer = true;
    //                     rhs = JsExpr::New(
    //                         vec!["RustInteger".to_string()],
    //                         vec![JsExpr::LitInt(lit_int.base10_parse::<i32>().unwrap())],
    //                     );
    //                 }
    //                 Lit::Float(_) => {}
    //                 Lit::Bool(lit_bool) => {
    //                     global_data.rust_prelude_types.bool = true;
    //                     rhs = JsExpr::New(
    //                         vec!["RustBool".to_string()],
    //                         vec![JsExpr::LitBool(lit_bool.value)],
    //                     )
    //                 }
    //                 Lit::Verbatim(_) => {}
    //                 _ => {}
    //             },
    //             _ => {}
    //         }
    //     }
    // } else {
    //     todo!()
    // }

    // TODO check *lhs as part of the above control flow
    // Check if lhs is a deref, in which case replace assignment with `.derefAssign()`
    // match &*expr_assign.left {
    //     Expr::Unary(expr_unary) => match &expr_unary.op {
    //         UnOp::Deref(_) => {
    //             return JsExpr::MethodCall(
    //                 Box::new(handle_expr(&*expr_assign.left, global_data, current_module).0),
    //                 "derefAssign".to_string(),
    //                 vec![rhs],
    //             );
    //         }
    //         _ => {}
    //     },
    //     _ => {}
    // }
}

/// NOTE handle_expr should never be called multiple times on the same expr, even if you are confident it won't mutate global_data twice, it will mess up the logging
/// -> (JsExpr, return type)
pub fn handle_expr(
    expr: &ExprRef,
    global_data: &mut GlobalData,
    current_module: &[String],
) -> (JsExpr, RustType2) {
    debug!("handle_expr");
    match expr {
        ExprRef::Array(expr_array) => {
            // TODO how to handle `let a: [i32, 0] = [];`? or other cases where there is no elements and the type is inferred from elsewhere

            let type_ = if !expr_array.elems.is_empty() {
                handle_expr(
                    expr_array.elems.first().unwrap(),
                    global_data,
                    current_module,
                )
                .1
            } else {
                // RustType::Todo
                todo!()
            };
            (
                JsExpr::Array(
                    expr_array
                        .elems
                        .iter()
                        .map(|elem| handle_expr(elem, global_data, current_module).0)
                        .collect::<Vec<_>>(),
                ),
                RustType2::Array(Box::new(type_)),
            )
        }
        ExprRef::Assign(expr_assign) => (
            handle_expr_assign(expr_assign, global_data, current_module),
            RustType2::Unit,
        ),
        ExprRef::Async(_) => todo!(),
        ExprRef::Await(expr_await) => {
            let js_expr = handle_expr(&expr_await.base, global_data, current_module);
            (JsExpr::Await(Box::new(js_expr.0)), js_expr.1)
        }
        ExprRef::Binary(expr_binary) => {
            // let span = debug_span!("handle_expr_binary", expr_binary = ?quote! { #expr_binary }.to_string());
            // let _guard = span.enter();

            // // TODO hack to not convert === to .eq() when comparing JS primitives
            // let primitive = match &*expr_binary.left {
            //     Expr::Field(expr_field) => match &expr_field.member {
            //         Member::Named(ident) => {
            //             ident == "js_number" || ident == "js_string" || ident == "js_bool"
            //         }
            //         Member::Unnamed(_) => false,
            //     },
            //     _ => false,
            // };

            // if primitive {
            //     return (JsExpr::Binary(
            //         Box::new(handle_expr(&*expr_binary.left, global_data, current_module).0),
            //         JsOp::from_binop(expr_binary.op),
            //         Box::new(handle_expr(
            //             &*expr_binary.right,
            //             global_data,
            //             current_module,
            //         ).0),
            //     ), RustType::Unknown);
            // }

            // TODO IMPORTANT for some reason which I have failed to debug, for the destructure_struct test, the following line seems cause a second "handle_expr_binary" tracing span to be entered, despite expr_binary.left definitely being a path.
            // let (lhs_expr, lhs_type) = match &*expr_binary.left {
            //     Expr::Path(expr_path) => {
            //         let (expr, partial_rust_type) =
            //             handle_expr_path(expr_path, global_data, current_module);
            //         match partial_rust_type {
            //             PartialRustType::StructIdent(_, _, _, _) => todo!(),
            //             PartialRustType::EnumVariantIdent(_, _, _, _, _) => todo!(),
            //             PartialRustType::RustType(rust_type, is_mut_var) => {
            //                 if is_mut_var && rust_type.is_js_primative() {
            //                     (
            //                         JsExpr::Field(Box::new(expr), Ident::Str("inner")),
            //                         rust_type,
            //                     )
            //                 } else {
            //                     (expr, rust_type)
            //                 }
            //             }
            //         }
            //     }
            //     _ => handle_expr(&expr_binary.left, global_data, current_module),
            // };

            // If we are updating the lhs must be mut var (or a derefferenced &mut)
            // assert!(lhs_is_mut_var);

            // let (rhs_expr, _rhs_type) =
            //     handle_expr(&expr_binary.right, global_data, current_module);

            let (mut lhs_expr, lhs_rust_type, lhs_is_mut_var) = match &*expr_binary.left {
                ExprRef::Path(expr_path) => {
                    let (expr, partial) = handle_expr_path(expr_path, global_data, current_module);
                    match partial {
                        PartialRustType::StructIdent(_, _) => todo!(),
                        PartialRustType::EnumVariantIdent(_, _, _) => todo!(),
                        PartialRustType::RustType(rust_type, is_mut_var, _is_var) => {
                            (expr, rust_type, is_mut_var)
                        }
                    }
                }
                _ => {
                    let (expr, rust_type) =
                        handle_expr(&expr_binary.left, global_data, current_module);
                    (expr, rust_type, false)
                }
            };
            // println!("{}", quote! { #expr_binary });
            // dbg!(&lhs_expr);
            // dbg!(&lhs_rust_type);
            // dbg!(&lhs_is_mut_var);

            let (mut rhs_expr, rhs_rust_type, rhs_is_mut_var) = match &*expr_binary.right {
                ExprRef::Path(expr_path) => {
                    let (expr, partial) = handle_expr_path(expr_path, global_data, current_module);
                    match partial {
                        PartialRustType::StructIdent(_, _) => todo!(),
                        PartialRustType::EnumVariantIdent(_, _, _) => todo!(),
                        PartialRustType::RustType(rust_type, is_mut_var, _is_var) => {
                            (expr, rust_type, is_mut_var)
                        }
                    }
                }
                _ => {
                    let (expr, rust_type) =
                        handle_expr(&expr_binary.right, global_data, current_module);
                    (expr, rust_type, false)
                }
            };

            let _lhs_is_mut_ref = matches!(lhs_rust_type, RustType2::MutRef(_));
            let _rhs_is_mut_ref = matches!(rhs_rust_type, RustType2::MutRef(_));

            if lhs_is_mut_var && lhs_rust_type.is_js_primative() {
                lhs_expr = JsExpr::Field(Box::new(lhs_expr), Ident::Str("inner"));
            }
            if rhs_is_mut_var && rhs_rust_type.is_js_primative() {
                rhs_expr = JsExpr::Field(Box::new(rhs_expr), Ident::Str("inner"));
            }

            // TODO need to check for `impl Add for Foo`
            let type_ = match expr_binary.op {
                BinOp::Add(_) | BinOp::Sub(_) | BinOp::Mul(_) | BinOp::Div(_) | BinOp::Rem(_) => {
                    lhs_rust_type.clone()
                }
                BinOp::And(_) | BinOp::Or(_) => RustType2::Bool,
                BinOp::BitXor(_) => todo!(),
                BinOp::BitAnd(_) => todo!(),
                BinOp::BitOr(_) => todo!(),
                BinOp::Shl(_) => todo!(),
                BinOp::Shr(_) => todo!(),
                BinOp::Eq(_)
                | BinOp::Lt(_)
                | BinOp::Le(_)
                | BinOp::Ne(_)
                | BinOp::Ge(_)
                | BinOp::Gt(_) => RustType2::Bool,
                BinOp::AddAssign(_)
                | BinOp::SubAssign(_)
                | BinOp::MulAssign(_)
                | BinOp::DivAssign(_)
                | BinOp::RemAssign(_) => RustType2::Unit,
                BinOp::BitXorAssign(_) => todo!(),
                BinOp::BitAndAssign(_) => todo!(),
                BinOp::BitOrAssign(_) => todo!(),
                BinOp::ShlAssign(_) => todo!(),
                BinOp::ShrAssign(_) => todo!(),
                _ => todo!(),
            };

            let method_name = match expr_binary.op {
                BinOp::Add(_) => "add",
                BinOp::Sub(_) => "sub",
                BinOp::Mul(_) => "mul",
                BinOp::Div(_) => "div",
                BinOp::Rem(_) => "rem",
                BinOp::And(_) => "bool_and",
                BinOp::Or(_) => "or",
                BinOp::BitXor(_) => todo!(),
                BinOp::BitAnd(_) => todo!(),
                BinOp::BitOr(_) => todo!(),
                BinOp::Shl(_) => todo!(),
                BinOp::Shr(_) => todo!(),
                BinOp::Eq(_) => "eq",
                BinOp::Lt(_) => "lt",
                BinOp::Le(_) => "le",
                BinOp::Ne(_) => "ne",
                BinOp::Ge(_) => "ge",
                BinOp::Gt(_) => "gt",
                BinOp::AddAssign(_) => "add_assign",
                BinOp::SubAssign(_) => "sub_assign",
                BinOp::MulAssign(_) => todo!(),
                BinOp::DivAssign(_) => todo!(),
                BinOp::RemAssign(_) => todo!(),
                BinOp::BitXorAssign(_) => todo!(),
                BinOp::BitAndAssign(_) => todo!(),
                BinOp::BitOrAssign(_) => todo!(),
                BinOp::ShlAssign(_) => todo!(),
                BinOp::ShrAssign(_) => todo!(),
                _ => todo!(),
            };

            let js_op = match expr_binary.op {
                BinOp::Add(_) => JsOp::Add,
                BinOp::Sub(_) => todo!(),
                BinOp::Mul(_) => todo!(),
                BinOp::Div(_) => todo!(),
                BinOp::Rem(_) => todo!(),
                BinOp::And(_) => todo!(),
                BinOp::Or(_) => todo!(),
                BinOp::BitXor(_) => todo!(),
                BinOp::BitAnd(_) => todo!(),
                BinOp::BitOr(_) => todo!(),
                BinOp::Shl(_) => todo!(),
                BinOp::Shr(_) => todo!(),
                BinOp::Eq(_) => JsOp::Eq,
                BinOp::Lt(_) => JsOp::Lt,
                BinOp::Le(_) => todo!(),
                BinOp::Ne(_) => todo!(),
                BinOp::Ge(_) => JsOp::GtEq,
                BinOp::Gt(_) => JsOp::Gt,
                BinOp::AddAssign(_) => JsOp::AddAssign,
                BinOp::SubAssign(_) => todo!(),
                BinOp::MulAssign(_) => todo!(),
                BinOp::DivAssign(_) => todo!(),
                BinOp::RemAssign(_) => todo!(),
                BinOp::BitXorAssign(_) => todo!(),
                BinOp::BitAndAssign(_) => todo!(),
                BinOp::BitOrAssign(_) => todo!(),
                BinOp::ShlAssign(_) => todo!(),
                BinOp::ShrAssign(_) => todo!(),
                _ => todo!(),
            };

            fn types_are_primative(
                rust_type: &RustType2,
                _impl_target_types: &[RustType2],
            ) -> bool {
                match rust_type {
                    RustType2::TypeParam(rust_type_param) => {
                        match &rust_type_param.type_ {
                            // TODO if one side is unresolved, then we should use the type of the other side to resovle it
                            RustTypeParamValue2::Unresolved => false,
                            RustTypeParamValue2::RustType(resolved_rust_type) => {
                                types_are_primative(resolved_rust_type, &[])
                            }
                        }
                    }
                    RustType2::I32 => true,
                    RustType2::F32 => true,
                    RustType2::Bool => true,
                    RustType2::String => true,
                    RustType2::Option(_) => todo!(),
                    RustType2::Result(_) => todo!(),
                    RustType2::MutRef(_) => todo!(),
                    RustType2::Ref(inner) => types_are_primative(inner, &[]),
                    RustType2::Fn(_, _, _) => todo!(),
                    _ => false,
                }
            }
            let lhs_is_primative =
                types_are_primative(&lhs_rust_type, &global_data.impl_block_target_type);

            let expr = if lhs_is_primative {
                JsExpr::Binary(Box::new(lhs_expr), js_op, Box::new(rhs_expr))
            } else {
                let lhs = Box::new(JsExpr::Paren(Box::new(lhs_expr)));
                JsExpr::MethodCall(lhs, Ident::Str(method_name), vec![rhs_expr])
            };
            (expr, type_)
        }
        ExprRef::Block(expr_block) => {
            handle_expr_block(expr_block, global_data, current_module, true, true)
        }
        ExprRef::Break(_) => (JsExpr::Break, RustType2::NotAllowed),
        ExprRef::Call(expr_call) => handle_expr_call(expr_call, global_data, current_module),
        ExprRef::Cast(_) => todo!(),
        ExprRef::Closure(expr_closure) => {
            handle_expr_closure(expr_closure, global_data, current_module, None)
        }
        ExprRef::Const(_) => todo!(),
        ExprRef::Continue(_) => todo!(),
        ExprRef::Field(expr_field) => {
            let (base_expr, base_type) = handle_expr(&expr_field.base, global_data, current_module);
            // TODO for a field, the type must be a struct or tuple, so look it up and get the type of the field
            match &expr_field.member {
                Member::Named(ident) => {
                    fn get_field_type(
                        base_type: RustType2,
                        global_data: &GlobalData,
                        ident: &syn::Ident,
                    ) -> RustType2 {
                        match base_type {
                            RustType2::StructOrEnum(_type_params, item_definition) => {
                                match &item_definition.struct_or_enum_info {
                                    StructEnumUniqueInfo2::Struct(struct_def_info) => {
                                        match &struct_def_info.fields {
                                            StructFieldInfo::UnitStruct => todo!(),
                                            StructFieldInfo::TupleStruct(_) => todo!(),
                                            StructFieldInfo::RegularStruct(fields) => fields
                                                .iter()
                                                .find_map(|(field_name, field_type)| {
                                                    (field_name == &ident.to_string()).then_some(
                                                        field_type
                                                            .clone()
                                                            .into_rust_type2(global_data),
                                                    )
                                                })
                                                .unwrap()
                                                .clone(),
                                        }
                                    }
                                    StructEnumUniqueInfo2::Enum(_) => todo!(),
                                }
                            }
                            RustType2::MutRef(inner_type) => {
                                get_field_type(*inner_type, global_data, ident)
                            }
                            _ => {
                                dbg!(&base_type);
                                todo!()
                            }
                        }
                    }
                    let field_type = get_field_type(base_type, global_data, ident);
                    (
                        JsExpr::Field(Box::new(base_expr), Ident::Syn(ident.clone())),
                        field_type,
                    )
                }
                Member::Unnamed(index) => {
                    fn get_unnamed_field_type(
                        base_type: RustType2,
                        global_data: &GlobalData,
                        index: &Index,
                    ) -> RustType2 {
                        match base_type {
                            RustType2::Tuple(tuple_types) => {
                                tuple_types[index.index as usize].clone()
                            }
                            RustType2::StructOrEnum(_type_params, item_definition) => {
                                match &item_definition.struct_or_enum_info {
                                    StructEnumUniqueInfo2::Struct(struct_def_info) => {
                                        match &struct_def_info.fields {
                                            StructFieldInfo::UnitStruct => todo!(),
                                            StructFieldInfo::TupleStruct(fields) => fields
                                                [index.index as usize]
                                                .clone()
                                                .into_rust_type2(global_data),
                                            StructFieldInfo::RegularStruct(_) => todo!(),
                                        }
                                    }
                                    StructEnumUniqueInfo2::Enum(_) => todo!(),
                                }
                            }
                            _ => {
                                dbg!(&base_type);
                                todo!()
                            }
                        }
                    }
                    let type_ = get_unnamed_field_type(base_type, global_data, index);
                    (
                        JsExpr::Index(
                            Box::new(base_expr),
                            Box::new(JsExpr::LitInt(index.index as i32)),
                        ),
                        type_,
                    )
                }
            }
        }
        ExprRef::ForLoop(expr_for_loop) => {
            // TODO handle multiple vars/patterns like `for (one, two, three) in data {}`
            let pat = match &*expr_for_loop.pat {
                Pat::Ident(pat_ident) => Ident::Syn(pat_ident.ident.clone()),
                _ => todo!(),
            };
            let name = match &*expr_for_loop.pat {
                Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
                _ => todo!(),
            };
            let mut_ = match &*expr_for_loop.pat {
                Pat::Ident(pat_ident) => pat_ident.mutability.is_some(),
                _ => todo!(),
            };

            let (iter_expr, iter_type) =
                handle_expr(&expr_for_loop.expr, global_data, current_module);

            let element_type = match iter_type {
                RustType2::Vec(element_type) => *element_type,
                RustType2::Array(element_type) => *element_type,
                RustType2::Tuple(_) => todo!(),
                _ => todo!(),
            };

            // let stmts = expr_for_loop
            //     .body
            //     .stmts
            //     .iter()
            //     .flat_map(|stmt| {
            //         handle_stmt(stmt, global_data, current_module)
            //             .into_iter()
            //             .map(|(stmt, type_)| stmt)
            //     })
            //     .collect();

            // Create new scope for fn vars
            global_data.scopes.push(GlobalDataScope {
                variables: Vec::new(),
                items: Vec::new(),
                _look_in_outer_scope: true,
                use_mappings: Vec::new(),
            });

            global_data
                .scopes
                .last_mut()
                .unwrap()
                .variables
                .push(ScopedVar {
                    name,
                    mut_,
                    type_: element_type,
                });

            let (stmts, _return_type) = parse_fn_body_stmts(
                false,
                false,
                false,
                &expr_for_loop.body,
                global_data,
                current_module,
            );

            // pop fn scope
            global_data.scopes.pop();

            (
                JsExpr::ForLoop(pat, Box::new(iter_expr), stmts),
                RustType2::Unit,
            )
        }
        ExprRef::Group(_) => todo!(),
        ExprRef::If(expr_if) => {
            match &*expr_if.cond {
                ExprRef::Let(_) => todo!(),
                _ => {}
            }

            global_data.scopes.push(GlobalDataScope {
                variables: Vec::new(),
                items: Vec::new(),
                _look_in_outer_scope: true,
                use_mappings: Vec::new(),
            });
            // TODO block needs to use something like parse_fn_body to be able to return the type

            // NOTE that like match expressions, we can't rely on a single block to get the return type since some might return never/unreachable, so we need to go through each block until we find a non never/unreachable type.
            // TODO For now assume first block return types
            let (succeed_stmts, types): (Vec<_>, Vec<RustType2>) = expr_if
                .then_branch
                .iter()
                .flat_map(|stmt| handle_stmt(stmt, global_data, current_module))
                .unzip();

            global_data.scopes.pop();
            // update_classes_js_stmts(&mut succeed_stmts, &scope.impl_blocks);

            // NOTE we need to directly handle fail blocks to ensure the block doesn't get converted to an IFFE which would happen if it we handled by handle_expr_block
            let fail = expr_if.else_branch.as_ref().map(|expr| {
                //
                match &**expr {
                    ExprRef::Block(expr_block) => {
                        // Box::new(handle_expr(&*expr, global_data, current_module).0)
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
                    ExprRef::If(_) => Box::new(handle_expr(expr, global_data, current_module).0),
                    _ => panic!(),
                }
            });

            // TODO handle same as expr::block

            (
                JsExpr::If(JsIf {
                    assignment: None,
                    declare_var: false,
                    condition: Box::new(handle_expr(&expr_if.cond, global_data, current_module).0),
                    succeed: succeed_stmts,
                    fail,
                }),
                types.last().unwrap().clone(),
            )
        }
        ExprRef::Index(expr_index) => {
            let (expr, type_) = handle_expr(&expr_index.expr, global_data, current_module);
            let (index_expr, _index_type) =
                handle_expr(&expr_index.index, global_data, current_module);
            // NOTE `Index` is a trait that can be implemented for any non primitive type (I think?), so need to look up the `Index` impl of the base expr's type to find what the `Output` type is
            // TODO we can use square bracket array[] indexing for arrays, but for other types which don't get transpiled to an array, we need to use `.index(i)` instead
            // "only traits defined in the current crate can be implemented for primitive types"
            // "only traits defined in the current crate can be implemented for arbitrary types"
            // "define and implement a trait or new type instead"
            let (rust_type, _use_square_brackets) = match type_ {
                RustType2::NotAllowed => todo!(),
                RustType2::Todo => todo!(),
                RustType2::Unit => todo!(),
                RustType2::I32 => todo!(),
                RustType2::F32 => todo!(),
                RustType2::Bool => todo!(),
                RustType2::String => todo!(),
                RustType2::StructOrEnum(_, _) => todo!(),
                RustType2::Vec(rust_type) => (*rust_type, true),
                RustType2::Array(rust_type) => (*rust_type, true),
                RustType2::Tuple(_) => todo!(),
                RustType2::MutRef(_) => todo!(),
                RustType2::Unknown => todo!(),
                RustType2::Never => todo!(),
                RustType2::Fn(_, _, _) => todo!(),
                RustType2::Option(_) => todo!(),
                RustType2::Result(_) => todo!(),
                RustType2::TypeParam(_) => todo!(),
                RustType2::ImplTrait(_) => todo!(),
                RustType2::UserType(_, _) => todo!(),
                RustType2::Ref(_) => todo!(),
                RustType2::Closure(_, _) => todo!(),
                RustType2::FnVanish => todo!(),
                RustType2::Box(_) => todo!(),
                RustType2::Self_ => todo!(),
            };
            (
                JsExpr::Index(Box::new(expr), Box::new(index_expr)),
                rust_type,
            )
        }
        ExprRef::Infer(_) => todo!(),
        ExprRef::Let(expr_let) => {
            dbg!(expr_let);
            todo!()
        }
        ExprRef::Lit(expr_lit) => match &expr_lit.lit {
            Lit::Str(lit_str) => {
                // global_data.rust_prelude_types.string = true;
                // JsExpr::New(
                //     vec!["RustString".to_string()],
                //     vec![JsExpr::LitStr(lit_str.value())],
                // )
                (JsExpr::LitStr(lit_str.value()), RustType2::String)
            }
            Lit::ByteStr(_) => todo!(),
            Lit::Byte(_) => todo!(),
            Lit::Char(_) => todo!(),
            Lit::Int(lit_int) => {
                // global_data.rust_prelude_types.integer = true;
                // JsExpr::New(
                //     vec!["RustInteger".to_string()],
                //     vec![JsExpr::LitInt(lit_int.base10_parse::<i32>().unwrap())],
                // )
                (
                    JsExpr::LitInt(lit_int.base10_parse::<i32>().unwrap()),
                    RustType2::I32,
                )
            }
            Lit::Float(lit_float) => {
                global_data.rust_prelude_types.float = true;
                (
                    JsExpr::New(
                        PathIdent::Single(Ident::Str("RustFloat")),
                        vec![JsExpr::LitFloat(lit_float.base10_parse::<f32>().unwrap())],
                    ),
                    RustType2::F32,
                )
            }
            Lit::Bool(lit_bool) => {
                // global_data.rust_prelude_types.bool = true;
                // JsExpr::New(
                //     vec!["RustBool".to_string()],
                //     vec![JsExpr::LitBool(lit_bool.value)],
                // )
                (JsExpr::LitBool(lit_bool.value), RustType2::Bool)
            }
            Lit::Verbatim(_) => todo!(),
            _ => todo!(),
        },
        ExprRef::Loop(expr_loop) => (
            JsExpr::While(
                Box::new(JsExpr::LitBool(true)),
                expr_loop
                    .body
                    .iter()
                    .flat_map(|stmt| {
                        handle_stmt(stmt, global_data, current_module)
                            .into_iter()
                            .map(|(stmt, _type_)| stmt)
                    })
                    .collect::<Vec<_>>(),
            ),
            RustType2::Never,
        ),
        ExprRef::Macro(expr_macro) => {
            handle_expr_and_stmt_macro(expr_macro, global_data, current_module)
        }
        ExprRef::Match(expr_match) => {
            handle_expr_match(expr_match, false, global_data, current_module)
        }
        ExprRef::MethodCall(expr_method_call) => {
            handle_expr_method_call(expr_method_call, global_data, current_module)
        }
        ExprRef::Paren(expr_paren) => {
            let (expr, type_) = handle_expr(&expr_paren.expr, global_data, current_module);
            (JsExpr::Paren(Box::new(expr)), type_)
        }
        ExprRef::Path(expr_path) => {
            // dbg!("handle_expr expr_path:");
            // dbg!(&expr_path);
            let (js_expr, partial_rust_type) =
                handle_expr_path(expr_path, global_data, current_module);
            // dbg!(&partial_rust_type);
            match partial_rust_type {
                // We don't allow `handle_expr()` to be call for tuple struct and enum variant (with args) instantiaion, instead they must must be handled within `handle_expr_call()`
                PartialRustType::StructIdent(_, _) => panic!(),
                PartialRustType::EnumVariantIdent(_, _, _) => panic!(),
                PartialRustType::RustType(rust_type, _is_mut_var, _is_var) => (js_expr, rust_type),
            }
        }
        ExprRef::Range(_) => todo!(),
        ExprRef::Reference(expr_reference) => {
            if expr_reference.mutability {
                // Vars need to know whether they are having a mut ref taken to know whether they should copy or not ie `var.inner`, `var.copy()`, or whatever. So we repeat the path handle from `handle_expr()` here so that we can explicitly pass a true argument for `is_having_mut_ref_taken` to handle_expr_path().
                // dbg!("yes");
                let (expr, rust_type) = match &*expr_reference.expr {
                    ExprRef::Path(expr_path) => {
                        let (js_expr, partial_rust_type) =
                            handle_expr_path(expr_path, global_data, current_module);
                        // dbg!(&expr_path);
                        // dbg!(&js_expr);
                        // dbg!(&partial_rust_type);
                        match partial_rust_type {
                            // We don't allow `handle_expr()` to be call for tuple struct and enum variant (with args) instantiaion, instead they must must be handled within `handle_expr_call()`
                            PartialRustType::StructIdent(_, _) => panic!(),
                            PartialRustType::EnumVariantIdent(_, _, _) => panic!(),
                            PartialRustType::RustType(rust_type, _is_mut_var, _is_var) => {
                                (js_expr, rust_type)
                            }
                        }
                    }
                    expr => {
                        let (js_expr, rust_type) = handle_expr(expr, global_data, current_module);

                        // We might already have primative types like RustType::String and RustType::Integer, but now that we are taking a `&mut` we need to ensure that the appropriate prelude flags are set
                        match rust_type {
                            RustType2::NotAllowed => todo!(),
                            RustType2::Unknown => todo!(),
                            RustType2::Todo => todo!(),
                            RustType2::Unit => todo!(),
                            RustType2::Never => todo!(),
                            RustType2::ImplTrait(_) => todo!(),
                            RustType2::TypeParam(_) => todo!(),
                            RustType2::I32 => {
                                global_data.rust_prelude_types.rust_integer = true;
                                (
                                    JsExpr::New(
                                        PathIdent::Single(Ident::Str("RustInteger")),
                                        vec![js_expr],
                                    ),
                                    rust_type,
                                )
                            }
                            RustType2::F32 => todo!(),
                            RustType2::Bool => todo!(),
                            RustType2::String => {
                                global_data.rust_prelude_types.rust_string = true;
                                (
                                    JsExpr::New(
                                        PathIdent::Single(Ident::Str("RustString")),
                                        vec![js_expr],
                                    ),
                                    rust_type,
                                )
                            }
                            RustType2::Option(_) => todo!(),
                            RustType2::Result(_) => todo!(),
                            RustType2::StructOrEnum(_, _) => (js_expr, rust_type),
                            RustType2::Vec(_) => todo!(),
                            RustType2::Array(_) => todo!(),
                            RustType2::Tuple(_) => todo!(),
                            RustType2::UserType(_, _) => todo!(),
                            RustType2::MutRef(_) => todo!(),
                            RustType2::Ref(_) => todo!(),
                            RustType2::Fn(_, _, _) => todo!(),
                            RustType2::Closure(_, _) => todo!(),
                            RustType2::FnVanish => todo!(),
                            RustType2::Box(_) => todo!(),
                            RustType2::Self_ => todo!(),
                        }
                    }
                };
                (expr, RustType2::MutRef(Box::new(rust_type)))
            } else {
                let (expr, rust_type) = match &*expr_reference.expr {
                    ExprRef::Path(expr_path) => {
                        let (expr, partial) =
                            handle_expr_path(expr_path, global_data, current_module);
                        match partial {
                            PartialRustType::StructIdent(_, _) => todo!(),
                            PartialRustType::EnumVariantIdent(_, _, _) => todo!(),
                            PartialRustType::RustType(rust_type, is_mut_var, _is_var) => {
                                if is_mut_var {
                                    (
                                        JsExpr::Field(Box::new(expr), Ident::Str("inner")),
                                        rust_type,
                                    )
                                } else {
                                    (expr, rust_type)
                                }
                            }
                        }
                    }
                    _ => handle_expr(&expr_reference.expr, global_data, current_module),
                };
                (expr, rust_type)
            }
        }
        ExprRef::Repeat(_) => todo!(),
        ExprRef::Return(expr_return) => {
            if let Some(expr) = &expr_return.expr {
                // If return is the deref of a &mut, then `.copy()` it
                let (js_expr, type_) = match &**expr {
                    ExprRef::Unary(expr_unary) => match expr_unary.op {
                        UnOp::Deref(_) => {
                            let (expr, type_) = handle_expr(expr, global_data, current_module);
                            (
                                JsExpr::MethodCall(Box::new(expr), Ident::Str("copy"), Vec::new()),
                                type_,
                            )
                        }
                        _ => handle_expr(expr, global_data, current_module),
                    },
                    _ => handle_expr(expr, global_data, current_module),
                };

                (JsExpr::Return(Box::new(js_expr)), type_)
            } else {
                // TODO surely an empty in Rust should also be an empty return in JS?
                todo!()
                // (JsExpr::Return(Box::new(JsExpr::Vanish)), RustType::Todo)
            }
        }
        ExprRef::Struct(expr_struct) => {
            // NOTE remember this handles both struct and enum variant instantiation eg `MyStruct {}` and `MyEnum::Variant { num: 0 }`
            // dbg!("handle expr_struct");
            // println!("{}", quote! { #expr_struct });
            let (js_path_expr, rust_partial_type) =
                handle_expr_path_inner(&expr_struct.path, global_data, current_module);

            // dbg!(&js_path_expr);
            // dbg!(&rust_partial_type);
            match rust_partial_type {
                PartialRustType::StructIdent(type_params, item_def) => {
                    let struct_def_info = match &item_def.struct_or_enum_info {
                        StructEnumUniqueInfo2::Struct(struct_def_info) => struct_def_info,
                        StructEnumUniqueInfo2::Enum(_) => panic!(),
                    };
                    let js_deduped_path = match js_path_expr.clone() {
                        JsExpr::Path(path) => {
                            assert_eq!(path.len(), 1);
                            path
                        }
                        _ => todo!(),
                    };
                    let rust_type = RustType2::StructOrEnum(type_params, item_def.clone());
                    let args = expr_struct
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let (js_expr, rust_type) =
                                handle_expr(&field.expr, global_data, current_module);
                            let field_is_mut = match &struct_def_info.fields {
                                StructFieldInfo::UnitStruct => panic!(),
                                StructFieldInfo::TupleStruct(rust_types) => match rust_types[i] {
                                    RustType::MutRef(_) => true,
                                    _ => false,
                                },
                                StructFieldInfo::RegularStruct(fields) => {
                                    let (_, rust_type) = &fields[i];
                                    match rust_type {
                                        RustType::MutRef(_) => true,
                                        _ => false,
                                    }
                                }
                            };
                            let arg_is_mut = match rust_type {
                                RustType2::MutRef(_) => true,
                                _ => false,
                            };
                            if !field_is_mut && arg_is_mut {
                                JsExpr::Field(Box::new(js_expr), Ident::Str("inner"))
                            } else {
                                js_expr
                            }
                        })
                        .collect::<Vec<_>>();
                    // TODO IMPORTANT need to be using deduplicated name here
                    (JsExpr::New(js_deduped_path, args), rust_type)
                }
                PartialRustType::EnumVariantIdent(type_params, item_def, _variant_name) => {
                    let rust_type = RustType2::StructOrEnum(type_params, item_def);
                    // TODO IMPORTANT need to be using deduplicated name here
                    // (JsExpr::New(vec![name], args), rust_type);
                    let obj = JsExpr::Object(
                        expr_struct
                            .fields
                            .iter()
                            .map(|field| {
                                (
                                    match &field.member {
                                        Member::Named(ident) => ident.to_string(),
                                        Member::Unnamed(_) => todo!(),
                                    },
                                    Box::new(
                                        handle_expr(&field.expr, global_data, current_module).0,
                                    ),
                                )
                            })
                            .collect::<Vec<_>>(),
                    );
                    (JsExpr::FnCall(Box::new(js_path_expr), vec![obj]), rust_type)
                }
                PartialRustType::RustType(_, _, _) => todo!(),
            }

            // if segs.len() == 2 {
            //     (
            //         JsExpr::FnCall(Box::new(JsExpr::Path(segs)), vec![obj]),
            //         rust_type,
            //     )
            // } else {
            //     let struct_name = segs.first().unwrap().clone();
            //     if struct_name == "FetchOptions" || struct_name == "SseOptions" {
            //         (obj, RustType::Todo)
            //     } else {
            //         // TODO we are assuming all other struct literals are inside ::new() so can be disappeared because the JsClass will write the constructor body
            //         // JsExpr::Vanish
            //         // TODO Expr structs can be instaniating an object but also instantiating an enum Variant with struct args. For now assume all Paths with len == 2 are enum variants and everthing else is a struct instaniation. Need an improved AST.
            //         let args = expr_struct
            //             .fields
            //             .iter()
            //             .map(|field| handle_expr(&field.expr, global_data, current_module).0)
            //             .collect::<Vec<_>>();

            //         // (JsExpr::New(vec![struct_name], args), RustType::Todo)
            //     }
            // }
        }
        ExprRef::Try(_) => todo!(),
        ExprRef::TryBlock(_) => todo!(),
        ExprRef::Tuple(_) => todo!(),
        ExprRef::Unary(expr_unary) => match expr_unary.op {
            UnOp::Deref(_) => {
                let (mut expr, rust_type, is_mut_var) = match &*expr_unary.expr {
                    ExprRef::Path(expr_path) => {
                        let (expr, partial) =
                            handle_expr_path(expr_path, global_data, current_module);
                        match partial {
                            PartialRustType::StructIdent(_, _) => todo!(),
                            PartialRustType::EnumVariantIdent(_, _, _) => todo!(),
                            PartialRustType::RustType(rust_type, is_mut_var, _is_var) => {
                                (expr, rust_type, is_mut_var)
                            }
                        }
                    }
                    _ => {
                        let (expr, rust_type) =
                            handle_expr(&expr_unary.expr, global_data, current_module);
                        (expr, rust_type, false)
                    }
                };

                let is_mut_ref = matches!(rust_type, RustType2::MutRef(_));

                if (is_mut_var || is_mut_ref) && rust_type.is_js_primative() {
                    expr = JsExpr::Field(Box::new(expr), Ident::Str("inner"));
                }

                // let thing = &*expr_unary.expr;
                // dbg!("let (expr, rust_type) = handle_expr(&*expr_unary.expr, global_data, current_module);");
                // println!("{}", quote! { #thing });
                // dbg!(&rust_type);
                // let add_one_fn_info = global_data.lookup_fn_info_known_module(
                //     &vec!["crate".to_string()],
                //     &Some(vec![1]),
                //     &"add_one".to_string(),
                // );
                // dbg!(add_one_fn_info);
                // If type is a mut ref, then convert to the "inner" type.
                match rust_type {
                    RustType2::MutRef(inner_type) => {
                        let add_inner = match *inner_type {
                            RustType2::NotAllowed => todo!(),
                            RustType2::Unknown => todo!(),
                            RustType2::Todo => todo!(),
                            RustType2::Unit => todo!(),
                            RustType2::Never => todo!(),
                            RustType2::ImplTrait(_) => todo!(),
                            RustType2::TypeParam(_) => todo!(),
                            RustType2::I32 => true,
                            RustType2::F32 => true,
                            RustType2::Bool => true,
                            RustType2::String => true,
                            RustType2::Option(_) => todo!(),
                            RustType2::Result(_) => todo!(),
                            RustType2::StructOrEnum(type_params, item_def) => {
                                dbg!(type_params);
                                dbg!(item_def);
                                todo!();
                            }
                            RustType2::Vec(_) => todo!(),
                            RustType2::Array(_) => todo!(),
                            RustType2::Tuple(_) => todo!(),
                            RustType2::UserType(_, _) => todo!(),
                            RustType2::MutRef(_) => todo!(),
                            RustType2::Ref(_) => todo!(),
                            RustType2::Fn(_, _, _) => todo!(),
                            RustType2::Closure(_, _) => todo!(),
                            RustType2::FnVanish => todo!(),
                            RustType2::Box(_) => todo!(),
                            RustType2::Self_ => todo!(),
                        };

                        let new_expr = if add_inner {
                            JsExpr::Field(Box::new(expr), Ident::Str("inner"))
                        } else {
                            expr
                        };
                        (new_expr, *inner_type)
                    }
                    // TODO I though we didn't care about & types, only &mut ???
                    RustType2::Ref(_) => todo!(),
                    rust_type => (expr, rust_type),
                }
            }
            UnOp::Not(_) => {
                let js_expr = JsExpr::Not(Box::new(
                    handle_expr(&expr_unary.expr, global_data, current_module).0,
                ));
                (js_expr, RustType2::Bool)
            }
            UnOp::Neg(_) => {
                let (expr, type_) = handle_expr(&expr_unary.expr, global_data, current_module);
                (JsExpr::Minus(Box::new(expr)), type_)
            }
            _ => todo!(),
        },
        ExprRef::Unsafe(expr_unsafe) => {
            handle_expr_block(&expr_unsafe.block, global_data, current_module, true, true)
        }
        ExprRef::Verbatim(_) => todo!(),
        ExprRef::While(expr_while) => (
            JsExpr::While(
                Box::new(handle_expr(&expr_while.cond, global_data, current_module).0),
                expr_while
                    .body
                    .iter()
                    .flat_map(|stmt| {
                        handle_stmt(stmt, global_data, current_module)
                            .into_iter()
                            .map(|(stmt, _type_)| stmt)
                    })
                    .collect::<Vec<_>>(),
            ),
            RustType2::Unit,
        ),
        ExprRef::Yield(_) => todo!(),
    }
}

fn handle_expr_closure(
    expr_closure: &RustExprClosure,
    global_data: &mut GlobalData,
    current_module: &[String],
    // (input types, return type)
    known_type: Option<(Vec<RustType2>, RustType2)>,
) -> (JsExpr, RustType2) {
    let async_ = matches!(&*expr_closure.body, ExprRef::Async(_));

    let body_is_js_block = match &*expr_closure.body {
        ExprRef::Async(expr_async) => {
            // If we have a single statement which is an expression that has no semi so is being returned then in Rust async we have to put it in a block but in Javascript we don't need to

            // multi lines should be in blocks
            let stmts = &expr_async.stmts;
            if stmts.len() > 1 {
                true
            } else {
                let is_expr_with_no_semi = match stmts.last().unwrap() {
                    StmtsRef::Expr(_, semi) => !semi,
                    StmtsRef::Macro(_) => todo!(),
                    _ => false,
                };
                !is_expr_with_no_semi
            }
        }
        ExprRef::Block(_) => true,
        ExprRef::Match(_) => true,
        _ => false,
    };

    let inputs = expr_closure
        .inputs
        .iter()
        .map(|input| match input {
            Pat::Ident(pat_ident) => Ident::Syn(pat_ident.ident.clone()),
            Pat::Tuple(_) => todo!(),
            Pat::Type(pat_type) => match &*pat_type.pat {
                Pat::Ident(pat_ident) => Ident::Syn(pat_ident.ident.clone()),
                _ => todo!(),
            },
            other => {
                dbg!(other);
                todo!()
            }
        })
        .collect::<Vec<_>>();

    // NOTE we don't add closure as a fn to scoped fns, like for fn definitions because we instead do this when the closure (or fn) is assigned to a variable

    // Create scope for closure body
    // NOTE the closure arguments are added to `GlobalDataScope.variables` in the for loop below. TODO can probably clean this up.
    // TODO IMPORTANT surely a closure body scope should look outside the scope so we should pass true??
    global_data.scopes.push(GlobalDataScope {
        variables: Vec::new(),
        items: Vec::new(),
        _look_in_outer_scope: false,
        use_mappings: Vec::new(),
    });

    // Below is copied/adapted from `handle_item_fn()`. Ideally we would use the same code for both but closure inputs are just pats because they have no self/reciever and might not even have a type eg `|x| x + 1`
    // record which vars are mut and/or &mut
    let mut copy_stmts = Vec::new();
    // for input in &item_fn.sig.inputs {
    //     match input {
    //         FnArg::Receiver(_) => {}
    //         FnArg::Typed(pat_type) => match &*pat_type.pat {
    //              ...
    //         },
    //     }
    // }

    let mut input_types = Vec::new();

    for (i, input) in expr_closure.inputs.iter().enumerate() {
        let (input_name, mutable, pat_type) = match input {
            Pat::Const(_) => todo!(),
            Pat::Ident(pat_ident) => (
                pat_ident.ident.to_string(),
                pat_ident.mutability.is_some(),
                None,
            ),
            Pat::Lit(_) => todo!(),
            Pat::Macro(_) => todo!(),
            Pat::Or(_) => todo!(),
            Pat::Paren(_) => todo!(),
            Pat::Path(_) => todo!(),
            Pat::Range(_) => todo!(),
            Pat::Reference(_) => todo!(),
            Pat::Rest(_) => todo!(),
            Pat::Slice(_) => todo!(),
            Pat::Struct(_) => todo!(),
            Pat::Tuple(_) => todo!(),
            Pat::TupleStruct(_) => todo!(),
            Pat::Type(pat_type) => {
                let (input_name, mutable) = match &*pat_type.pat {
                    Pat::Ident(pat_ident) => {
                        (pat_ident.ident.to_string(), pat_ident.mutability.is_some())
                    }
                    _ => todo!(),
                };
                (input_name, mutable, Some(*pat_type.ty.clone()))
            }
            Pat::Verbatim(_) => todo!(),
            Pat::Wild(_) => todo!(),
            _ => todo!(),
        };

        let input_type = if let Some(pat_type) = pat_type {
            parse_fn_input_or_field(&pat_type, mutable, &Vec::new(), current_module, global_data)
        } else if let Some(known_type) = &known_type {
            known_type.0[i].clone()
        } else {
            todo!();
            // Could in theory return Uknown and resolve the type later, but even Rust will often error with:
            // `type must be known at this point. consider giving this closure parameter an explicit type.`
            // So need think more about which cases need type annotations and which don't, so just assume type annotations for now.
            RustType2::Unknown
        };

        input_types.push(input_type.clone());

        let scoped_var = ScopedVar {
            name: input_name.clone(),
            mut_: mutable,
            type_: input_type.clone(),
        };

        // record add var to scope
        global_data
            .scopes
            .last_mut()
            .unwrap()
            .variables
            .push(scoped_var);

        // a mut input of a copy type like `mut num: i32` must be converted to `RustInteger`
        if mutable {
            copy_stmts.push(JsStmt::Local(JsLocal {
                public: false,
                export: false,
                type_: LocalType::Var,
                lhs: LocalName::Single(Ident::String(input_name.clone())),
                // value: JsExpr::MethodCall(
                //     Box::new(JsExpr::Path(vec![pat_ident.ident.to_string()])),
                //     "copy".to_string(),
                //     Vec::new(),
                // ),
                value: match &input_type {
                    RustType2::Todo => todo!(),
                    RustType2::Unit => todo!(),
                    RustType2::I32 => {
                        global_data.rust_prelude_types.rust_integer = true;
                        JsExpr::New(
                            PathIdent::Single(Ident::Str("RustInteger")),
                            vec![JsExpr::MethodCall(
                                Box::new(JsExpr::Path(PathIdent::Single(Ident::String(
                                    input_name,
                                )))),
                                Ident::Str("inner"),
                                Vec::new(),
                            )],
                        )
                    }
                    RustType2::F32 => todo!(),
                    RustType2::Bool => todo!(),
                    RustType2::String => todo!(),
                    RustType2::StructOrEnum(_, _) => todo!(),
                    RustType2::NotAllowed => todo!(),
                    RustType2::Unknown => todo!(),
                    RustType2::Never => todo!(),
                    RustType2::Vec(_) => todo!(),
                    RustType2::Array(_) => todo!(),
                    RustType2::Tuple(_) => todo!(),
                    RustType2::MutRef(_) => todo!(),
                    RustType2::Fn(_, _, _) => todo!(),
                    RustType2::Option(_) => todo!(),
                    RustType2::Result(_) => todo!(),
                    RustType2::TypeParam(_) => todo!(),
                    RustType2::ImplTrait(_) => todo!(),
                    RustType2::UserType(_, _) => todo!(),
                    RustType2::Ref(_) => todo!(),
                    RustType2::Closure(_, _) => todo!(),
                    RustType2::FnVanish => todo!(),
                    RustType2::Box(_) => todo!(),
                    RustType2::Self_ => todo!(),
                },
            }))
        }
    }

    // Need to handle different Expr's separately because Expr::Match needs passing an arg that it is being returned. Not sure the other cases are necessary
    let (body_stmts, return_type) = match &*expr_closure.body {
        ExprRef::Block(expr_block) => parse_fn_body_stmts(
            true,
            false,
            true,
            &expr_block.stmts,
            global_data,
            current_module,
        ),
        ExprRef::Async(expr_async) => parse_fn_body_stmts(
            true,
            false,
            true,
            &expr_async.stmts,
            global_data,
            current_module,
        ),
        ExprRef::Match(expr_match) => {
            let (expr, type_) = handle_expr_match(expr_match, true, global_data, current_module);
            (vec![JsStmt::Expr(expr, false)], type_)
        }
        other => {
            let (expr, type_) = handle_expr(other, global_data, current_module);
            (vec![JsStmt::Expr(expr, false)], type_)
        }
    };

    global_data.scopes.pop();

    (
        JsExpr::ArrowFn(async_, body_is_js_block, inputs, body_stmts),
        // NOTE closures cannot be generic
        // RustType::Fn(None, Vec::new(), None, RustTypeFnType::Standalone(())),
        RustType2::Closure(input_types, Box::new(return_type)),
    )
}

// TODO might want to split this up so that in some cases we can return JsStmt and JsExpr in others
pub fn handle_expr_and_stmt_macro(
    mac: &RustExprOrStmtMacro,
    global_data: &mut GlobalData,
    current_module: &[String],
) -> (JsExpr, RustType2) {
    let mac = mac.mac.clone();
    let path_segs = mac
        .path
        .segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect::<Vec<_>>();

    let span = debug_span!("handle_expr_and_stmt_macro", Macro = ?quote! { #mac }.to_string());
    let _guard = span.enter();

    // dbg!("handle_expr_and_stmt_macro");
    // println!("{}", quote! { #mac });

    if path_segs.len() == 1 {
        if path_segs[0] == "vec" {
            let input = mac.tokens.clone().to_string();
            let expr_array = syn::parse_str::<syn::ExprArray>(&format!("[{input}]")).unwrap();
            let vec_type = if let Some(elem) = expr_array.elems.first() {
                // IMPORTANT need to be careful about calling handle_expr an element twice like here in case information is added to global_data.scope twice, or similar problems? duplicates shouldn't cause problems for scope data?
                handle_expr(
                    // TODO IMPORTANT should be parsing macro tokens in update/make_item_definitions/extract_data/etc so we don't have to use (, &mut vec![], &None, &mut vec![]) here.
                    &expr_to_expr_ref(elem.clone(), &mut vec![], &None, &mut vec![]),
                    global_data,
                    current_module,
                )
                .1
            } else {
                RustType2::Unknown
            };
            let expr_vec = expr_array
                .elems
                .iter()
                .map(|elem| {
                    handle_expr(
                        &expr_to_expr_ref(elem.clone(), &mut vec![], &None, &mut vec![]),
                        global_data,
                        current_module,
                    )
                    .0
                })
                .collect::<Vec<_>>();
            return (JsExpr::Array(expr_vec), RustType2::Vec(Box::new(vec_type)));
        }
        if path_segs[0] == "panic" {
            let input = mac.tokens.clone().to_string();
            // TODO use a custom error so it isn't inadvertently caught by other JS code in the app?
            return (JsExpr::ThrowError(input), RustType2::Never);
        }
        if path_segs[0] == "try_" {
            let input = mac.tokens.clone().to_string();
            let try_block = syn::parse_str::<syn::Block>(&input).unwrap();
            let stmt_vec = try_block
                .stmts
                .iter()
                .flat_map(|stmt| {
                    handle_stmt(
                        &stmt_to_stmts_ref(stmt.clone(), &mut vec![], &None, &mut vec![]),
                        global_data,
                        current_module,
                    )
                    .into_iter()
                    .map(|(stmt, _type_)| stmt)
                })
                .collect();
            return (JsExpr::TryBlock(stmt_vec), RustType2::Unit);
        }
        if path_segs[0] == "catch" {
            let input = mac.tokens.clone().to_string();
            let mut parts = input.split(',');
            let err_var_name = parts.next().unwrap();
            let err_var_name = syn::parse_str::<syn::Ident>(err_var_name)
                .unwrap()
                .to_string();
            let _err_var_type = parts.next().unwrap();
            let catch_block = parts.collect::<String>();
            let catch_block = syn::parse_str::<syn::Block>(&catch_block).unwrap();
            let stmt_vec = catch_block.stmts.into_iter().flat_map(|stmt| {
                handle_stmt(
                    &stmt_to_stmts_ref(stmt, &mut vec![], &None, &mut vec![]),
                    global_data,
                    current_module,
                )
                .into_iter()
                .map(|(stmt, _type_)| stmt)
            });
            let stmt_vec = stmt_vec.collect::<Vec<_>>();
            return (JsExpr::CatchBlock(err_var_name, stmt_vec), RustType2::Unit);
        }
        if path_segs[0] == "assert" {
            let input = mac.tokens.clone().to_string();
            let condition_expr = syn::parse_str::<syn::Expr>(&input).unwrap();

            // TODO
            let bool_is_mut = false;
            warn!("before");
            let condition_js = if bool_is_mut {
                JsExpr::Field(
                    Box::new(JsExpr::Paren(Box::new(
                        handle_expr(
                            &expr_to_expr_ref(condition_expr, &mut vec![], &None, &mut vec![]),
                            global_data,
                            current_module,
                        )
                        .0,
                    ))),
                    Ident::Str("jsBoolean"),
                )
            } else {
                handle_expr(
                    &expr_to_expr_ref(condition_expr, &mut vec![], &None, &mut vec![]),
                    global_data,
                    current_module,
                )
                .0
            };
            warn!("after");

            return (
                JsExpr::MethodCall(
                    Box::new(JsExpr::Path(PathIdent::Single(Ident::Str("console")))),
                    Ident::Str("assert"),
                    vec![condition_js],
                ),
                RustType2::Unit,
            );
        }
        if path_segs[0] == "assert_eq" {
            let input = mac.tokens.clone().to_string();
            let mut parts = input.split(',');

            let lhs = parts.next().unwrap();
            let syn_lhs = syn::parse_str::<syn::Expr>(lhs).unwrap();
            let lhs = handle_expr(
                &expr_to_expr_ref(syn_lhs.clone(), &mut vec![], &None, &mut vec![]),
                global_data,
                current_module,
            );

            let rhs = parts.next().unwrap();
            let rhs = syn::parse_str::<syn::Expr>(rhs).unwrap();
            let rhs = handle_expr(
                &expr_to_expr_ref(rhs, &mut vec![], &None, &mut vec![]),
                global_data,
                current_module,
            );

            // let equality_check = JsExpr::Binary(Box::new(lhs), JsOp::Eq, Box::new(rhs));
            // Check if we have primatives so can use === otherwise use .eq()
            let (lhs_is_mut, lhs_is_mut_ref, lhs_is_primative) = match syn_lhs {
                Expr::Path(expr_path) => {
                    if expr_path.path.segments.len() == 1 {
                        let var_name = expr_path.path.segments.first().unwrap().ident.to_string();
                        global_data
                            .scopes
                            .iter()
                            .rev()
                            .find_map(|s| {
                                s.variables
                                    .iter()
                                    .rev()
                                    .find(|scoped_var| scoped_var.name == var_name)
                            })
                            .map_or(
                                (false, false, false),
                                |ScopedVar { mut_, type_, .. }| {
                                    let is_primative = match type_ {
                                        RustType2::Todo => {
                                            // global_data
                                            //     .rust_prelude_types
                                            //     .number_prototype_extensions = true;
                                            // global_data
                                            //     .rust_prelude_types
                                            //     .string_prototype_extensions = true;
                                            false
                                        }
                                        RustType2::Unit => true,
                                        RustType2::I32 => true,
                                        RustType2::F32 => true,
                                        RustType2::Bool => true,
                                        RustType2::String => true,
                                        RustType2::StructOrEnum(_, _) => false,
                                        RustType2::NotAllowed => false,
                                        RustType2::Unknown => false,
                                        RustType2::Never => false,
                                        RustType2::Vec(_) => false,
                                        RustType2::Array(_) => false,
                                        RustType2::Tuple(_) => false,
                                        RustType2::MutRef(_) => false,
                                        RustType2::Fn(_, _, _) => false,
                                        RustType2::Option(_) => false,
                                        RustType2::Result(_) => false,
                                        RustType2::TypeParam(_) => false,
                                        RustType2::ImplTrait(_) => false,
                                        RustType2::UserType(_, _) => todo!(),
                                        RustType2::Ref(inner) => match &**inner {
                                            RustType2::String => true,
                                            _ => todo!(),
                                        },
                                        RustType2::Closure(_, _) => todo!(),
                                        RustType2::FnVanish => todo!(),
                                        RustType2::Box(_) => todo!(),
                                        RustType2::Self_ => todo!(),
                                    };
                                    let mut_ref = matches!(type_, RustType2::MutRef(_));
                                    (*mut_, mut_ref, is_primative)
                                },
                            )
                    } else {
                        (false, false, false)
                    }
                }
                _ => (false, false, false),
            };

            let mut equality_check = if !lhs_is_primative || lhs_is_mut || lhs_is_mut_ref {
                // global_data.rust_prelude_types.number_prototype_extensions = true;
                // global_data.rust_prelude_types.string_prototype_extensions = true;
                JsExpr::MethodCall(Box::new(lhs.0), Ident::Str("eq"), vec![rhs.0])
            } else {
                JsExpr::Binary(Box::new(lhs.0), JsOp::Eq, Box::new(rhs.0))
            };

            // let equality_check = JsExpr::Binary(Box::new(lhs), JsOp::Eq, Box::new(rhs));
            let bool_is_mut = false;
            if bool_is_mut {
                equality_check = JsExpr::Field(
                    Box::new(JsExpr::Paren(Box::new(equality_check))),
                    Ident::Str("jsBoolean"),
                );
            }
            return (
                JsExpr::MethodCall(
                    Box::new(JsExpr::Path(PathIdent::Single(Ident::Str("console")))),
                    Ident::Str("assert"),
                    vec![equality_check],
                ),
                RustType2::Unit,
            );
        }
    }
    todo!()
}

fn handle_expr_method_call(
    expr_method_call: &RustExprMethodCall,
    global_data: &mut GlobalData,
    current_module: &[String],
) -> (JsExpr, RustType2) {
    let _method_name = expr_method_call.method.to_string();
    // let span = debug_span!("handle_expr_method_call", expr_method_call = ?quote! { #expr_method_call }.to_string());
    // let _guard = span.enter();

    // // Look up types... should really be looking up item types when handling the receiver, and then at this stage we are just seeing if there are generics we can resolve with the argument types
    // // Look up variable
    // let scoped_var = global_data
    //     .scopes
    //     .iter()
    //     .rev()
    //     .find_map(|s| s.variables.iter().rev().find(|v| v.name == var_name))
    //     .cloned();

    // // Look for module level structs and enums
    // let module = global_data
    //     .modules
    //     .iter()
    //     .find(|module| &module.path == current_module);
    // let struct_or_enum =
    //     module.and_then(|m| m.item_definitons.iter().find(|se| se.ident == var_name));
    // let module_level_method =
    //     struct_or_enum.and_then(|se| se.members.iter().find(|m| m.ident == method_name));
    // // End of type lookup

    // JS doesn't allow eg `1.foo()` so need to check for literals and wrap them in parens like `(1).foo()`
    // TODO also want to check for other things like sting literals etc
    let receiver_needs_parens = match &*expr_method_call.receiver {
        ExprRef::Array(_) => todo!(),
        ExprRef::Assign(_) => todo!(),
        ExprRef::Async(_) => todo!(),
        ExprRef::Await(_) => todo!(),
        ExprRef::Binary(_) => todo!(),
        ExprRef::Block(_) => todo!(),
        ExprRef::Break(_) => todo!(),
        ExprRef::Call(_) => false,
        ExprRef::Cast(_) => todo!(),
        ExprRef::Closure(_) => todo!(),
        ExprRef::Const(_) => todo!(),
        ExprRef::Continue(_) => todo!(),
        ExprRef::Field(_) => false,
        ExprRef::ForLoop(_) => todo!(),
        ExprRef::Group(_) => todo!(),
        ExprRef::If(_) => todo!(),
        ExprRef::Index(_) => todo!(),
        ExprRef::Infer(_) => todo!(),
        ExprRef::Let(_) => todo!(),
        ExprRef::Lit(_expr_lit) => {
            // match expr_lit.lit {
            //     Lit::Str(_) => todo!(),
            //     Lit::ByteStr(_) => todo!(),
            //     Lit::CStr(_) => todo!(),
            //     Lit::Byte(_) => todo!(),
            //     Lit::Char(_) => todo!(),
            //     Lit::Int(_) => todo!(),
            //     Lit::Float(_) => todo!(),
            //     Lit::Bool(_) => todo!(),
            //     Lit::Verbatim(_) => todo!(),
            //     _ => todo!(),
            // }
            true
        }
        ExprRef::Loop(_) => todo!(),
        ExprRef::Macro(_) => todo!(),
        ExprRef::Match(_) => todo!(),
        ExprRef::MethodCall(_) => false,
        ExprRef::Paren(_) => todo!(),
        ExprRef::Path(_) => false,
        ExprRef::Range(_) => todo!(),
        ExprRef::Reference(_) => todo!(),
        ExprRef::Repeat(_) => todo!(),
        ExprRef::Return(_) => todo!(),
        ExprRef::Struct(_) => todo!(),
        ExprRef::Try(_) => todo!(),
        ExprRef::TryBlock(_) => todo!(),
        ExprRef::Tuple(_) => todo!(),
        ExprRef::Unary(_) => todo!(),
        ExprRef::Unsafe(_) => todo!(),
        ExprRef::Verbatim(_) => todo!(),
        ExprRef::While(_) => todo!(),
        ExprRef::Yield(_) => todo!(),
    };
    let mut method_name = expr_method_call.method.to_string();

    // !!!!!!!!!!!!!
    // Want to know if method is `&mut self` so we can prevent `handle_expr_path` adding `.inner`, but we don't know the method info until we have parsed the receiver so we can look up the method!
    // It feels like handle_expr_path automatically adding `.inner` is wrong and should just return and expression for the path and then the parent handler should determine whether to wrap it with a `.inner`?
    // let (receiver, receiver_type) = match &*expr_method_call.receiver {
    //     Expr::Path(expr_path) => {
    //         // TODO we are using a hack here by just using `is_having_mut_ref_taken = true` to force no `.inner` but really we should have a proper input or rename `is_having_mut_ref_taken`
    //         let (expr, partial_rust_type) =
    //             handle_expr_path(expr_path, global_data, current_module, true);
    //         let rust_type = match partial_rust_type {
    //             PartialRustType::StructIdent(_, _, _, _) => todo!(),
    //             PartialRustType::EnumVariantIdent(_, _, _, _, _) => todo!(),
    //             PartialRustType::RustType(rust_type) => rust_type,
    //         };
    //         (expr, rust_type)
    //     }
    //     _ => handle_expr(&expr_method_call.receiver, global_data, current_module),
    // };

    // Handling `document()` -> `document`:

    let (mut receiver, receiver_type, receiver_is_mut_var) = match &*expr_method_call.receiver {
        ExprRef::Path(expr_path) => {
            let (expr, partial) = handle_expr_path(expr_path, global_data, current_module);
            match partial {
                PartialRustType::StructIdent(_, _) => todo!(),
                PartialRustType::EnumVariantIdent(_, _, _) => todo!(),
                PartialRustType::RustType(type_, is_mut_var, _is_var) => (expr, type_, is_mut_var),
            }
        }
        _ => {
            let (expr, type_) =
                handle_expr(&expr_method_call.receiver, global_data, current_module);
            (expr, type_, false)
        }
    };
    // dbg!(&receiver_type);

    // TASK we want to get the type of the method arguments so we can pass it to `handle_expr_closure` (and possibly other handlers) when we create args_js_exprs and args_rust_types. The problem is that we want to use args_rust_types to get the type of the method arguments (specifically for resolving type params)

    // NOTE CONTEXT a closure often doesn't have it's input types specified, typically because it is an argument and the type of the *input* provides more info about the closure type. However, what if the input type also uses generics? eg for vec.iter().map() the input type for the closure is generic and determined by what .iter() (the receiver) (which is also generic) resolves to. We might also eg specify the type of the closure when assiging it to a local???? (not common though). So we do need to attempt *some* type param resolving before passing the impl item types to `handle_expr_closure`.

    let method_turbofish_rust_types = expr_method_call.turbofish.as_ref().map(|generics| {
        generics
            .args
            .iter()
            .map(|generic_arg| match generic_arg {
                GenericArgument::Lifetime(_) => todo!(),
                GenericArgument::Type(type_) => {
                    parse_fn_input_or_field(
                        type_,
                        false,
                        // TODO include any receiver type params here
                        &[],
                        current_module,
                        global_data,
                    )
                }
                GenericArgument::Const(_) => todo!(),
                GenericArgument::AssocType(_) => todo!(),
                GenericArgument::AssocConst(_) => todo!(),
                GenericArgument::Constraint(_) => todo!(),
                _ => todo!(),
            })
            .collect::<Vec<_>>()
    });

    let sub_path = RustPathSegment2 {
        ident: method_name.clone(),
        turbofish: method_turbofish_rust_types.clone().unwrap_or_default(),
    };

    // Matches the receiver_type and returns the receiver's (possibly resolved) type params, and looks up the method's impl item
    // let (method_input_types, method_return_type) = get_method_input_and_return_types(
    let (receiver_type_params, method_impl_item) = get_receiver_params_and_method_impl_item(
        receiver_type.clone(),
        &method_name,
        global_data,
        // &receiver,
        // expr_method_call,
        // current_module,
        &sub_path,
    );

    let (_static, fn_info) = match method_impl_item.item {
        RustImplItemItemNoJs::Fn(static_, fn_info) => (static_, fn_info),
        RustImplItemItemNoJs::Const => todo!(),
    };

    // Now that we have the method impl item, we can get the method input types, and if any of them contain type params, we can attempt to resolve them from receiver_type_params or method_turbofish_rust_types
    let resolved_input_types = fn_info
        .inputs_types
        .iter()
        .map(|(_is_self, _is_mut, _name, input_type)| {
            resolve_input_type(
                input_type,
                &receiver_type,
                &receiver_type_params,
                &fn_info.generics,
                &method_turbofish_rust_types,
                global_data,
            )
        })
        .collect::<Vec<_>>();

    // Lookup method impl to see if any of it's args are type params

    // Parse the args
    let (args_js_exprs, _args_rust_types): (Vec<_>, Vec<_>) = {
        // NOTE resolved_input_types includes self inputs which are of course don't appear in args, so we must ignore the first item from resolved_input_types if it is a self
        let first_input_is_self = fn_info
            .inputs_types
            .first()
            .map(|input| input.0)
            .unwrap_or(false);

        let resolved_input_types_slice = if first_input_is_self {
            &resolved_input_types[1..]
        } else {
            &resolved_input_types[..]
        };
        expr_method_call
            .args
            .iter()
            .zip(resolved_input_types_slice)
            .map(|(arg, resolved_input_type)| {
                match arg {
                    ExprRef::Closure(expr_closure) => {
                        // This particular method input is a closure
                        let closure_types = match resolved_input_type {
                            RustType2::TypeParam(_) => todo!(),
                            RustType2::Fn(_, _, _) => todo!(),
                            RustType2::Closure(input_types, return_type) => {
                                (input_types.clone(), *return_type.clone())
                            }
                            RustType2::Option(_) => {
                                // how can a Expr::Closure arg have a RustType::Option resolved_input_type?
                                todo!()
                            }
                            _ => {
                                dbg!(resolved_input_type);
                                todo!()
                            }
                        };
                        handle_expr_closure(
                            expr_closure,
                            global_data,
                            current_module,
                            Some(closure_types),
                        )
                    }
                    _ => handle_expr(arg, global_data, current_module),
                }
            })
            .unzip()
    };

    // Now get the method return type from the impl item and similarly if it is, or contains, type params then see if it/they have been resolved by:
    // the receiver type params, method turbofish, arguments, (or within the body of the fn??)
    let method_return_type = fn_info.return_type.clone().into_rust_type2(global_data);

    // Update any resolved type params in the `method_return_type`
    // TODO resolve nested type params eg within a struct or Vec
    let method_return_type = match &method_return_type {
        RustType2::Option(_) => todo!(),
        RustType2::TypeParam(rust_type_param) => {
            // Check to see if type param has been resolved by parent/receiver type
            let parent_type_params = match &receiver_type {
                RustType2::TypeParam(_) => todo!(),
                RustType2::Option(inner) => Some(vec![inner.clone()]),
                RustType2::Result(_) => todo!(),
                // TODO
                // RustType2::StructOrEnum(type_params, _) => Some(type_params.clone()),
                RustType2::StructOrEnum(type_params, _) => None,
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
                _ => None,
            };
            // TODO type param might be defined on the method itself, so we need to look at turbofish and arg types
            // NOTE it could be that the receiver type is unresovled but the static fn_info def return type is resolved?
            if let Some(parent_type_params) = parent_type_params {
                let found_param = parent_type_params
                    .into_iter()
                    .find(|p| p.name == rust_type_param.name)
                    .unwrap();
                let found_param_is_resolved = match found_param.type_ {
                    RustTypeParamValue2::Unresolved => false,
                    RustTypeParamValue2::RustType(_) => true,
                };
                if found_param_is_resolved {
                    RustType2::TypeParam(found_param.clone())
                } else {
                    method_return_type
                }
            } else {
                method_return_type
            }
        }
        _ => method_return_type,
    };

    let method_takes_mut_ref_self =
        fn_info
            .inputs_types
            .first()
            .is_some_and(|(is_self, _is_mut, _name, type_)| {
                *is_self && matches!(type_, RustType::MutRef(_))
            });

    // Finally we see if the final types we get from parsing the arguments (after having already attempted to resolve generics using receiver type params, method turbofish, (anything else?)) we check that if the argument the same type param as the return type and is now resolved, we can update the return type???
    // let resolved_types = match method_impl_item.item {
    //     RustImplItemItemNoJs::Fn(private, static_, fn_info) => {
    //         // If return type has generics, replace them with the concretised generics that exist on either:
    //         // the receiver item, the method's turbofish, or the methods arguments
    //         let return_type = resolve_generics_for_return_type(
    //             fn_info.return_type.clone(),
    //             &receiver_type_params,
    //             &fn_info,
    //             &method_turbofish_rust_types,
    //             &args_rust_types,
    //         );

    //         (fn_info.inputs_types.clone(), return_type)
    //     }
    //     RustImplItemItemNoJs::Const => todo!(),
    // };

    if let JsExpr::Path(path) = &receiver {
        if path.len() == 2 {
            match path {
                PathIdent::PathTwo(path) => {
                    if path[0] == "JSON" && path[1] == "parse" {
                        // function parse(text) {try { return Result.Ok(JSON.parse(text)); } catch(err) { return Result.Err(err) }}
                        let body = "try { return Result.Ok(JSON.parse(text)); } catch(err) { return Result.Err(err) }".to_string();
                        return (
                            JsExpr::FnCall(
                                Box::new(JsExpr::Paren(Box::new(JsExpr::Fn(JsFn {
                                    iife: false,
                                    public: false,
                                    export: false,
                                    async_: false,
                                    is_method: false,
                                    name: Ident::Str("jsonParse"),
                                    input_names: vec![Ident::Str("text")],
                                    body_stmts: vec![JsStmt::Raw(body)],
                                })))),
                                expr_method_call
                                    .args
                                    .iter()
                                    .map(|arg| handle_expr(arg, global_data, current_module).0)
                                    .collect::<Vec<_>>(),
                            ),
                            RustType2::Todo,
                        );
                    }
                }
                _ => {}
            }
        }
    }

    if method_name.len() > 3 && &method_name[0..3] == "js_" {
        method_name = method_name[3..].to_string();
    }

    if let Some(last_char) = method_name.chars().last() {
        if last_char.is_ascii_digit() {
            method_name.pop().unwrap();
            // method_name = method_name[..method_name.len() - 1].to_string();
        }
    }

    // Waiting for this to be able to match on boxed variants: https://github.com/rust-lang/rust/issues/87121
    match &receiver_type {
        // RustType2::I32 => {}
        // RustType2::F32 => {}
        // RustType2::Bool => {}
        RustType2::String if method_name == "to_string" || method_name == "clone" => {
            // NOTE ASSUMPTION if the receiver a type RustType::String and also has a RustString wrapper (and this needs `.inner` calling) then it the receiver *must* be a `mut` var. Whilst receivers other than a `mut` var can have a RustString wrapper and call `.to_string()`, `.clone()`, etc, all those receivers *must* be a RustType::MutRef(RustType::String), which is handled below.
            // If receiver var has a RustString wrapper ie is `mut`, then take the inner
            if receiver_is_mut_var {
                (
                    JsExpr::Field(Box::new(receiver.clone()), Ident::Str("inner")),
                    RustType2::String,
                )
            } else {
                (receiver.clone(), RustType2::String)
            }
        }
        RustType2::String if method_name == "push_str" => {
            assert_eq!(args_js_exprs.len(), 1);
            if receiver_is_mut_var {
                (
                    JsExpr::Binary(
                        Box::new(JsExpr::Field(
                            Box::new(receiver.clone()),
                            Ident::Str("inner"),
                        )),
                        JsOp::AddAssign,
                        Box::new(args_js_exprs[0].clone()),
                    ),
                    RustType2::String,
                )
            } else {
                panic!("can't call push_str on a var which is not mutable")
            }
        }
        RustType2::Option(_) if fn_info.ident == "is_some_and" => {
            global_data.rust_prelude_types.option_is_some_and = true;
            let mut args = vec![receiver];
            args.extend(args_js_exprs);
            (
                JsExpr::FnCall(Box::new(JsExpr::Var("optionIsSomeAnd".to_string())), args),
                method_return_type,
            )
        }
        RustType2::Option(_) if fn_info.ident == "unwrap" => {
            global_data.rust_prelude_types.option_unwrap = true;
            (
                JsExpr::FnCall(
                    Box::new(JsExpr::Var("optionUnwrap".to_string())),
                    vec![receiver],
                ),
                method_return_type,
            )
        }
        // RustType2::Result(_) => {}
        RustType2::Vec(_) if method_name == "iter" || method_name == "collect" => {
            (receiver, method_return_type)
        }
        RustType2::Array(_) if method_name == "iter" || method_name == "collect" => {
            (receiver, method_return_type)
        }
        // RustType2::Box(_) => {}
        RustType2::MutRef(inner) if **inner == RustType2::String => {
            if method_name == "to_string" || method_name == "clone" {
                // Receiver is &mut so has a RustString wrapper so take the inner
                (
                    JsExpr::Field(Box::new(receiver.clone()), Ident::Str("inner")),
                    RustType2::String,
                )
            } else if method_name == "push_str" {
                assert_eq!(args_js_exprs.len(), 1);
                (
                    JsExpr::Binary(
                        Box::new(receiver.clone()),
                        JsOp::AddAssign,
                        Box::new(args_js_exprs[0].clone()),
                    ),
                    RustType2::String,
                )
            } else {
                todo!()
            }
        }
        // RustType2::Ref(_) => {}
        RustType2::NotAllowed => todo!(),
        RustType2::Unknown => todo!(),
        RustType2::Todo => todo!(),
        RustType2::Self_ => todo!(),
        RustType2::Unit => todo!(),
        RustType2::Never => todo!(),
        RustType2::ImplTrait(_) => todo!(),
        RustType2::TypeParam(_) => todo!(),
        RustType2::StructOrEnum(_, struct_enum_def)
            if struct_enum_def.ident == "Something" && expr_method_call.method == "cast" =>
        {
            assert!(args_js_exprs.is_empty());
            (receiver, method_return_type)
        }
        RustType2::StructOrEnum(_, struct_enum_def)
            if struct_enum_def.ident == "Document"
                && expr_method_call.method == "create_element_div" =>
        {
            assert!(args_js_exprs.is_empty());
            (
                JsExpr::MethodCall(
                    Box::new(receiver),
                    Ident::Str("create_element"),
                    vec![JsExpr::LitStr("div".to_string())],
                ),
                method_return_type,
            )
        }
        RustType2::Tuple(_) => todo!(),
        RustType2::UserType(_, _) => todo!(),
        // RustType2::Fn(_, _, fn_def) => {}
        RustType2::FnVanish => todo!(),
        RustType2::Closure(_, _) => todo!(),
        _ => {
            if receiver_is_mut_var && receiver_type.is_js_primative() && !method_takes_mut_ref_self
            {
                receiver = JsExpr::Field(Box::new(receiver), Ident::Str("inner"));
            }

            if receiver_needs_parens {
                receiver = JsExpr::Paren(Box::new(receiver));
            }

            (
                JsExpr::MethodCall(
                    Box::new(receiver),
                    Ident::Syn(expr_method_call.method.clone()),
                    args_js_exprs,
                ),
                method_return_type,
            )
        }
    }
}

/// When handling a method call, after we have looked up the method input types, which may contain type params, we call this function to update any type params with resolved type params from the reciever or method turbofish
fn resolve_input_type(
    input_type: &RustType,
    _receiver_type: &RustType2,
    receiver_type_params: &Vec<RustTypeParam2>,
    method_def_generics: &[String],
    method_turbofish_rust_types: &Option<Vec<RustType2>>,
    global_data: &GlobalData,
) -> RustType2 {
    match input_type {
        RustType::ImplTrait(_) => todo!(),
        RustType::TypeParam(rust_type_param) => {
            match rust_type_param.type_ {
                RustTypeParamValue::Unresolved => {}
                // We are updating the FnInfo definition inputs, so they should all be unresolved
                RustTypeParamValue::RustType(_) => panic!(),
            }
            // Look for type param in receiver type params
            if let Some(receiver_type_param) = receiver_type_params
                .iter()
                .find(|p| p.name == rust_type_param.name)
            {
                // If receiver type param is resolved, then return it
                match receiver_type_param.type_ {
                    RustTypeParamValue2::Unresolved => {}
                    RustTypeParamValue2::RustType(_) => {
                        return RustType2::TypeParam(receiver_type_param.clone());
                    }
                }
            }

            // Look for type param in method turbofish
            // For now we are assuming the the method turbofish migh be a mixture of types and _ like `::<i32, _>`. Not sure what _ gets parsed to at the mo.
            if let Some(method_turbofish_rust_types) = method_turbofish_rust_types {
                // We need to get the generics def for the method, and then see if any of those generics are used for inputs or the return type, and then check the type in method_turbofish_rust_types is not `_`.
                if let Some(pos) = method_def_generics
                    .iter()
                    .position(|g| g == &rust_type_param.name)
                {
                    // TODO only return if rust type is not `_`
                    return method_turbofish_rust_types[pos].clone();
                }
            }

            // No matches against receiver_type_params or method_turbofish_rust_types so just return original type
            input_type.clone().into_rust_type2(global_data)
        }
        RustType::Option(_) => todo!(),
        RustType::Result(_) => todo!(),
        RustType::StructOrEnum(_type_params, _module_path, _scope_id, _name) => {
            // We are trying to stop using RustType::ParentItem, so this could be `self` in which case we can safely use the receiver type params
            // Or it could be the parent/Self type but maybe with different type params?!?!
            // Or it can just be any other item enum, which also might be using the parent/Self/target's type params, ie type params defined in the impl block

            // TODO
            // let updated_type_params = type_params.clone().into_iter().map(|tp| {
            //     match tp.type_{
            //         RustTypeParamValue::Unresolved => todo!(),
            //         RustTypeParamValue::RustType(_) => tp,
            //     }
            // }).collect();

            // This isn't acutally doing anything so just convert to RustType2 instead
            // let updated_type_params = type_params.clone();
            // RustType::StructOrEnum(
            //     updated_type_params,
            //     module_path.clone(),
            //     scope_id.clone(),
            //     name.clone(),
            // )
            input_type.clone().into_rust_type2(global_data)
        }
        RustType::Vec(_) => todo!(),
        RustType::Array(_) => todo!(),
        RustType::Tuple(_) => todo!(),
        RustType::Box(_) => todo!(),
        RustType::UserType(_, _) => todo!(),
        RustType::MutRef(inner) => {
            let resolved_inner = resolve_input_type(
                inner,
                _receiver_type,
                receiver_type_params,
                method_def_generics,
                method_turbofish_rust_types,
                global_data,
            );
            RustType2::MutRef(Box::new(resolved_inner))
        }
        RustType::Ref(_) => todo!(),
        RustType::Fn(_, _, _, _, _) => todo!(),
        RustType::FnVanish => todo!(),
        RustType::Closure(input_types, return_type) => {
            let resolved_input_types = input_types
                .iter()
                .map(|input_type| {
                    resolve_input_type(
                        input_type,
                        _receiver_type,
                        receiver_type_params,
                        method_def_generics,
                        method_turbofish_rust_types,
                        global_data,
                    )
                })
                .collect();
            let resolved_return_type = resolve_input_type(
                return_type,
                _receiver_type,
                receiver_type_params,
                method_def_generics,
                method_turbofish_rust_types,
                global_data,
            );
            RustType2::Closure(resolved_input_types, Box::new(resolved_return_type))
        }
        _ => input_type.clone().into_rust_type2(global_data),
    }
}

/// Attempts to get the input types and return type of the method.
/// Also resolve the type if the definition uses a type param, which is why args_rust_types is required, so that if an input is a type param, we can resolve it to the type of the argument passed.
/// TODO only the handling for receiver_type = RustType::StructOrEnum attempts to resolve type params, need to add this to handling of other RustTypes
/// (input types, return type)
// fn get_method_input_and_return_types(

/// Matches the receiver_type and returns the receiver's (possibly resolved) type params, and looks up the method's impl item
fn get_receiver_params_and_method_impl_item(
    receiver_type: RustType2,
    method_name: &String,
    global_data: &GlobalData,
    // receiver: &JsExpr,
    // expr_method_call: &ExprMethodCall,
    // current_module: &Vec<String>,
    sub_path: &RustPathSegment2,
    // args_rust_types: Vec<RustType>,
    // ) -> (Vec<RustType>, RustType) {
) -> (Vec<RustTypeParam2>, RustImplItemNoJs) {
    match receiver_type {
        RustType2::NotAllowed => todo!(),
        RustType2::Unknown => todo!(),
        RustType2::Todo => todo!(),
        RustType2::Unit => todo!(),
        RustType2::Never => todo!(),
        RustType2::ImplTrait(_) => todo!(),
        RustType2::TypeParam(_) => todo!(),
        RustType2::I32 => {
            // TODO we want to be able to look up method return types in the same way we do for user structs, because we can do eg `impl Foo for i32 {}` so this seems like more evidence that we shouldn't distinguish between rust types?

            let i32_def = global_data.get_prelude_item_def("i32");
            (
                Vec::new(),
                global_data
                    .lookup_impl_item_item2(&i32_def, sub_path)
                    .unwrap(),
            )
            // match impl_method.item {
            //     RustImplItemItemNoJs::Fn(_, _, fn_info) => {
            //         (fn_info.inputs_types, fn_info.return_type)
            //     }
            //     RustImplItemItemNoJs::Const => todo!(),
            // }
        }
        RustType2::F32 => todo!(),
        RustType2::Bool => todo!(),
        RustType2::String => {
            // if method_name == "to_string" || method_name == "clone" {
            //     return (receiver.clone(), RustType::String);
            // } else {
            //     todo!()
            // }
            // if method_name == "to_string" || method_name == "clone" {
            //     // (Vec::new(), RustType::String)
            //     // todo!();
            //     gl
            // } else if method_name == "push_str" {
            //     // (vec![RustType::String], RustType::String)
            //     todo!();
            // } else {
            //     todo!()
            // }

            let string_def = global_data.get_prelude_item_def("String");

            // dbg!(&string_def);
            // dbg!(&sub_path);
            // dbg!(&global_data.impl_blocks_simpl);

            (
                Vec::new(),
                global_data
                    .lookup_impl_item_item2(&string_def, sub_path)
                    .unwrap(),
            )
        }
        RustType2::Option(type_param) => {
            let item_def = global_data.get_prelude_item_def("Option");
            // let type_param = match &*type_param {
            //     RustType::TypeParam(rust_type_param) => rust_type_param.clone(),
            //     other_rust_type => RustTypeParam {
            //         name: item_def.generics.first().unwrap().clone(),
            //         type_: RustTypeParamValue::RustType(Box::new(other_rust_type.clone())),
            //     },
            // };
            (
                vec![type_param],
                global_data
                    .lookup_impl_item_item2(&item_def, sub_path)
                    .unwrap(),
            )
        }
        RustType2::Result(_) => todo!(),
        RustType2::StructOrEnum(item_type_params, item_def) => {
            // dbg!(&item_def);
            (
                item_type_params,
                global_data
                    .lookup_impl_item_item2(&item_def, sub_path)
                    .unwrap(),
            )

            // match impl_method.item {
            //     RustImplItemItemNoJs::Fn(private, static_, fn_info) => {
            //         // If return type has generics, replace them with the concretised generics that exist on the receiver item, the method's turbofish, or the methods arguments
            //         let return_type = resolve_generics_for_return_type(
            //             fn_info.return_type.clone(),
            //             &item_type_params,
            //             &fn_info,
            //             &method_turbofish_rust_types,
            //             &args_rust_types,
            //         );

            //         (fn_info.inputs_types.clone(), return_type)
            //     }
            //     RustImplItemItemNoJs::Const => todo!(),
            // }
        }
        RustType2::Vec(_element) => {
            // TODO we are assuming `.collect::<Vec<_>>()` here but should support other `.collect()`s
            // if method_name == "iter" || method_name == "collect" {
            //     // (Vec::new(), RustType::Vec(element))
            //     todo!();
            // } else if method_name == "map" {
            //     // let closure_return = match &args_rust_types[0] {
            //     //     RustType::MutRef(_) => todo!(),
            //     //     RustType::Ref(_) => todo!(),
            //     //     RustType::Fn(_, _, _, _, _) => todo!(),
            //     //     RustType::Closure(return_type) => return_type.clone(),
            //     //     _ => todo!(),
            //     // };
            //     // (
            //     //     vec![RustType::Closure(closure_return.clone())],
            //     //     RustType::Vec(closure_return),
            //     // )
            //     todo!();
            // } else {
            //     todo!()
            // }

            let vec_def = global_data.get_prelude_item_def("Vec");
            // dbg!(&string_def);
            // dbg!(&sub_path);
            // dbg!(&global_data.impl_blocks_simpl);

            (
                Vec::new(),
                global_data
                    .lookup_impl_item_item2(&vec_def, sub_path)
                    .unwrap(),
            )
        }
        RustType2::Array(_element) => {
            // TODO need to think about the different between Array and Vec here
            // TODO we are assuming `.collect::<Vec<_>>()` here but should support other `.collect()`s
            if method_name == "iter" || method_name == "collect" {
                // (Vec::new(), RustType::Array(element))
                todo!();
            } else if method_name == "map" {
                // let closure_return = match &args_rust_types[0] {
                //     RustType::MutRef(_) => todo!(),
                //     RustType::Ref(_) => todo!(),
                //     RustType::Fn(_, _, _, _, _) => todo!(),
                //     RustType::Closure(return_type) => return_type.clone(),
                //     _ => todo!(),
                // };
                // (
                //     vec![RustType::Closure(closure_return.clone())],
                //     RustType::Array(closure_return),
                // )
                todo!();
            } else {
                todo!()
            }
        }
        RustType2::Tuple(_) => todo!(),
        RustType2::UserType(_, _) => todo!(),
        RustType2::MutRef(inner_rust_type) => {
            // todo!();
            get_receiver_params_and_method_impl_item(
                *inner_rust_type,
                method_name,
                global_data,
                // receiver,
                // expr_method_call,
                // current_module,
                sub_path,
            )
        }
        RustType2::Ref(_) => todo!(),
        RustType2::Fn(_, _, _) => todo!(),
        RustType2::Closure(_, _) => todo!(),
        RustType2::FnVanish => todo!(),
        RustType2::Box(_) => todo!(),
        RustType2::Self_ => todo!(),
    }
}

fn _resolve_generics_for_return_type(
    return_type: RustType,
    item_type_params: &Vec<RustTypeParam>,
    fn_info: &FnDef,
    method_turbofish_rust_types: &Option<Vec<RustType>>,
    args_rust_types: &Vec<RustType>,
) -> RustType {
    match return_type {
        RustType::NotAllowed => todo!(),
        RustType::Unknown => todo!(),
        RustType::Todo => todo!(),
        RustType::Unit => RustType::Unit,
        RustType::Never => todo!(),
        RustType::ImplTrait(_) => todo!(),
        RustType::TypeParam(rust_type_param) => {
            // type param is unresolved at this point
            assert!(match rust_type_param.type_ {
                RustTypeParamValue::Unresolved => true,
                RustTypeParamValue::RustType(_) => false,
            });

            _method_return_type_generic_resolve_to_rust_type(
                item_type_params,
                &rust_type_param,
                fn_info,
                method_turbofish_rust_types,
                args_rust_types,
            )
        }
        RustType::I32 => RustType::I32,
        RustType::F32 => RustType::F32,
        RustType::Bool => RustType::Bool,
        RustType::String => RustType::String,
        RustType::Option(_) => todo!(),
        RustType::Result(_) => todo!(),
        RustType::StructOrEnum(type_params, module_path, scope_id, name) => {
            // Return type generics are unresolved at this point - NO the return type might be eg `Foo<i32>` ie *resolved* generics.
            // assert!(type_params.iter().all(|tp| match tp.type_ {
            //     RustTypeParamValue::Unresolved => true,
            //     RustTypeParamValue::RustType(_) => false,
            // }));

            // So we have the *unresolved* RustType return type of the method call
            // If this type has any generics, we want to resolve them with the concrete type we already have from:
            // receiver item instance, the method's turbofish, and the methods arguments
            // So we need to take the return type def param names, see whether they belong to the item or method...
            // For the item and turbofish this is easy because it is clear what the type belong to, for method args, we need to check if there is any generic input types, (NO look to see if the generic name belongs to the item or method) *whose name matches a generic name in the return type*, and if it does just get the concrete type from the method arg and resolve the return type here.
            let resolved_type_params = type_params
                .iter()
                .map(|tp| {
                    match tp.type_ {
                        RustTypeParamValue::Unresolved => {
                            let mut new_tp = tp.clone();
                            let rust_type = _method_return_type_generic_resolve_to_rust_type(
                                item_type_params,
                                tp,
                                fn_info,
                                method_turbofish_rust_types,
                                args_rust_types,
                            );
                            new_tp.type_ = RustTypeParamValue::RustType(Box::new(rust_type));
                            new_tp
                        }
                        // type param is already resolved so don't need to do anything
                        RustTypeParamValue::RustType(_) => tp.clone(),
                    }
                })
                .collect::<Vec<_>>();

            RustType::StructOrEnum(
                resolved_type_params,
                module_path.clone(),
                scope_id.clone(),
                name,
            )
        }
        RustType::Vec(_) => todo!(),
        RustType::Array(_) => todo!(),
        RustType::Tuple(_) => todo!(),
        RustType::UserType(_, _) => todo!(),
        RustType::MutRef(_) => todo!(),
        RustType::Ref(inner_type) => RustType::Ref(Box::new(_resolve_generics_for_return_type(
            *inner_type,
            item_type_params,
            fn_info,
            method_turbofish_rust_types,
            args_rust_types,
        ))),
        RustType::Fn(_, _, _, _, _) => todo!(),
        RustType::Closure(_, _) => todo!(),
        RustType::FnVanish => todo!(),
        RustType::Box(_) => todo!(),
        RustType::Self_ => todo!(),
    }
}

/// For a type param return type, attempt to find a concrete type by looking: on the receiver's type params, the method's type params, or if the type param is used for one of the inputs, use the concrete type of the arg
fn _method_return_type_generic_resolve_to_rust_type(
    item_type_params: &[RustTypeParam],
    return_type_param: &RustTypeParam,
    fn_info: &FnDef,
    method_turbofish_rust_types: &Option<Vec<RustType>>,
    args_rust_types: &[RustType],
) -> RustType {
    // Is type param defined on item?
    if let Some(item_type_param) = item_type_params
        .iter()
        .find(|item_type_param| item_type_param.name == return_type_param.name)
    {
        match &item_type_param.type_ {
            RustTypeParamValue::Unresolved => todo!(),
            RustTypeParamValue::RustType(rust_type) => *rust_type.clone(),
        }
        // Is type param specified on method (and we have a method turbofish)?
    } else if let Some(gen_index) = fn_info.generics.iter().position(|method_type_param| {
        method_type_param == &return_type_param.name && method_turbofish_rust_types.is_some()
    }) {
        method_turbofish_rust_types.as_ref().unwrap()[gen_index].clone()

        // Is type param concrete value resolved by input argument?
    } else if let Some(pos) =
        fn_info
            .inputs_types
            .iter()
            .position(|(_is_self, _is_mut, _name, input_type)| match input_type {
                RustType::TypeParam(input_rust_type_param) => {
                    input_rust_type_param.name == return_type_param.name
                }
                // TODO we might have input types like eg Foo<T>
                _ => false,
            })
    {
        args_rust_types[pos].clone()
    } else {
        todo!()
    }
}

pub fn handle_expr_block(
    expr_block: &RustExprBlock,
    global_data: &mut GlobalData,
    current_module: &[String],
    allow_convert_to_iffe: bool,
    // This is simply to allow the else block of an if statement to prevent return being added, since we use assignment to temp var instead
    // TODO remove this
    allow_return: bool,
) -> (JsExpr, RustType2) {
    // let span = debug_span!("handle_expr_block", expr_block = ?quote! { #expr_block }.to_string());
    let span = debug_span!("handle_expr_block");
    let _guard = span.enter();

    // Because item definitions can appear after they are used, we can't simply add them to the scope as they are handled. We instead need to go through all the stmts and record the defined items before we do a second pass to actually handle/parse the stmts.
    let scoped_item_defs = expr_block
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
    // if !at_module_top_level {
    //     let scope = global_data.scopes.last_mut().unwrap();
    //     scope.items.push(index);
    // }

    global_data.scopes.push(GlobalDataScope {
        variables: Vec::new(),
        items: scoped_item_defs,
        _look_in_outer_scope: true,
        use_mappings: Vec::new(),
    });
    // TODO block needs to use something like parse_fn_body to be able to return the type
    // let (stmts, types): (Vec<_>, Vec<_>) = expr_block
    //     .block
    //     .stmts
    //     .iter()
    //     .flat_map(|stmt| handle_stmt(stmt, global_data, current_module))
    //     .unzip();
    let (stmts, return_type) = parse_fn_body_stmts(
        false,
        true,
        allow_return,
        &expr_block.stmts,
        global_data,
        current_module,
    );

    // pop block scope
    global_data.scopes.pop();
    // update_classes_js_stmts(&mut stmts, &scope.unwrap().impl_blocks);

    let block_returns_value = match stmts.last().unwrap() {
        JsStmt::Expr(JsExpr::Return(_), closing_semi) => {
            assert!(closing_semi);
            true
        }
        JsStmt::Expr(_expr, closing_semi) => !closing_semi,
        _ => false,
    };

    if block_returns_value && allow_convert_to_iffe {
        let iffe = JsExpr::FnCall(
            Box::new(JsExpr::Paren(Box::new(JsExpr::ArrowFn(
                false,
                true,
                Vec::new(),
                stmts,
            )))),
            Vec::new(),
        );
        (iffe, return_type)
    } else {
        (JsExpr::Block(stmts), return_type)
    }
}

fn handle_expr_call(
    expr_call: &RustExprCall,
    global_data: &mut GlobalData,
    current_module: &[String],
) -> (JsExpr, RustType2) {
    // dbg!(expr_call);

    let js_primitive = match &*expr_call.func {
        ExprRef::Path(expr_path) => {
            if expr_path.path.segments.len() == 1 {
                let ident = &expr_path.path.segments.first().unwrap().ident;
                ident == "JsNumber" || ident == "JsString" || ident == "JsBoolean"
            } else {
                false
            }
        }
        _ => false,
    };
    if js_primitive {
        return handle_expr(expr_call.args.first().unwrap(), global_data, current_module);
    }

    fn parse_args(
        expr_call: &RustExprCall,
        fn_inputs: Option<&[RustType]>,
        global_data: &mut GlobalData,
        current_module: &[String],
    ) -> (Vec<JsExpr>, Vec<RustType2>) {
        fn parse_args_inner(
            arg_expr: &ExprRef,
            input_is_mut_ref: bool,
            // Arg is &mut
            arg_is_mut_ref: bool,
            global_data: &mut GlobalData,
            current_module: &[String],
        ) -> (JsExpr, RustType2) {
            match arg_expr {
                ExprRef::Path(expr_path) => {
                    let (expr, partial) = handle_expr_path(expr_path, global_data, current_module);
                    match partial {
                        PartialRustType::StructIdent(_, _) => todo!(),
                        PartialRustType::EnumVariantIdent(_, _, _) => todo!(),
                        PartialRustType::RustType(rust_type, is_mut_var, _is_var) => {
                            if arg_is_mut_ref && input_is_mut_ref {
                                (expr, rust_type)
                            } else if is_mut_var {
                                (
                                    JsExpr::Field(Box::new(expr), Ident::Str("inner")),
                                    rust_type,
                                )
                            } else {
                                (expr, rust_type)
                            }
                        }
                    }
                }
                ExprRef::Reference(expr_reference) => parse_args_inner(
                    &expr_reference.expr,
                    input_is_mut_ref,
                    expr_reference.mutability,
                    global_data,
                    current_module,
                ),
                _ => handle_expr(arg_expr, global_data, current_module),
            }
        }

        if let Some(fn_inputs) = fn_inputs {
            expr_call
                .args
                .iter()
                .zip(fn_inputs)
                .map(|(arg_expr, input_type)| {
                    parse_args_inner(
                        arg_expr,
                        matches!(input_type, RustType::MutRef(_)),
                        false,
                        global_data,
                        current_module,
                    )
                })
                .unzip()
        } else {
            expr_call
                .args
                .iter()
                .map(|arg_expr| {
                    parse_args_inner(arg_expr, false, false, global_data, current_module)
                })
                .unzip()
        }
    }

    let (mut args_js_expr, mut args_rust_types) =
        parse_args(expr_call, None, global_data, current_module);

    // Determine return type, looking up structs/enums in scope and checking for generics and whether they can be inferred
    // TODO also need to handle looking up fns
    // TODO clean up handle_expr_local where this was before

    // parse fn call
    match &*expr_call.func {
        ExprRef::Path(expr_path) => {
            // if let Some(js_expr) = hardcoded_conversions(expr_path, args_js_expr.clone()) {
            //     return js_expr;
            // }

            let (expr, partial_rust_type) =
                handle_expr_path(expr_path, global_data, current_module);

            let rust_type = match partial_rust_type.clone() {
                // If a struct or enum variant is called, it must be a tuple strut of enum variant
                PartialRustType::StructIdent(type_params, item_def) => {
                    let struct_def = match &item_def.struct_or_enum_info {
                        StructEnumUniqueInfo2::Struct(struct_def) => struct_def,
                        StructEnumUniqueInfo2::Enum(_) => panic!(),
                    };

                    // Do any of the args to the tuple struct constructor resolve any of it's generics?
                    let updated_type_params = type_params
                        .iter()
                        .map(|tp| {
                            let field_types = match &struct_def.fields {
                                StructFieldInfo::UnitStruct => return tp.clone(),
                                StructFieldInfo::TupleStruct(field_types) => field_types.clone(),
                                StructFieldInfo::RegularStruct(named_field_types) => {
                                    named_field_types
                                        .iter()
                                        .cloned()
                                        .map(|(_name, type_)| type_)
                                        .collect::<Vec<_>>()
                                }
                            };
                            let mut new_tp = tp.clone();
                            for (i, field_type) in field_types.iter().enumerate() {
                                let matches_gen = match field_type {
                                    RustType::TypeParam(type_param) => type_param.name == tp.name,
                                    _ => false,
                                };
                                if matches_gen {
                                    new_tp.type_ = RustTypeParamValue2::RustType(Box::new(
                                        args_rust_types[i].clone(),
                                    ));
                                    return new_tp;
                                }
                            }
                            tp.clone()
                        })
                        .collect::<Vec<_>>();

                    RustType2::StructOrEnum(updated_type_params, item_def)
                }
                PartialRustType::EnumVariantIdent(type_params, item_def, variant_name) => {
                    let enum_def = match &item_def.struct_or_enum_info {
                        StructEnumUniqueInfo2::Struct(_) => panic!(),
                        StructEnumUniqueInfo2::Enum(enum_def) => enum_def,
                    };
                    let enum_variant = enum_def
                        .members
                        .iter()
                        .find(|m| m.ident == variant_name)
                        .unwrap();

                    // Do any of the args for the enum variant resolve any of the enum's generics?
                    let updated_type_params = type_params
                        .iter()
                        .map(|tp| {
                            for (i, input) in enum_variant.inputs.iter().enumerate() {
                                let input_type = match input {
                                    EnumVariantInputsInfo::Named { input_type, .. } => input_type,
                                    EnumVariantInputsInfo::Unnamed(input_type) => input_type,
                                };
                                let matches_gen = match input_type {
                                    RustType::TypeParam(type_param) => type_param.name == tp.name,
                                    _ => false,
                                };
                                if matches_gen {
                                    return RustTypeParam2 {
                                        name: tp.name.clone(),
                                        type_: RustTypeParamValue2::RustType(Box::new(
                                            args_rust_types[i].clone(),
                                        )),
                                    };
                                }
                            }
                            tp.clone()
                        })
                        .collect::<Vec<_>>();

                    // TODO we should be using references to ItemDefinitions so that we can check for prelude items by comparing pointer addresses
                    if item_def.ident == "Option" && variant_name == "Some" {
                        assert_eq!(args_js_expr.len(), 1);
                        assert_eq!(args_rust_types.len(), 1);
                        RustType2::Option(RustTypeParam2 {
                            // TODO don't hardcode "T"
                            name: "T".to_string(),
                            type_: RustTypeParamValue2::RustType(Box::new(
                                args_rust_types.first().unwrap().clone(),
                            )),
                        })
                    } else {
                        RustType2::StructOrEnum(updated_type_params, item_def)
                    }
                }
                PartialRustType::RustType(rust_type, _is_mut_var, _is_var) => {
                    // handle_expr_path checks if the path is any scoped/module level fn, enum variant, tuple struct, associated fn, or var with one of these types, but it doesn't know the args the path is being called with so it is at this point that we check if any generics can be made concrete
                    // We also resolved the type to whatever the call returns eg fn path -> fn return type, enum variant path -> enum instance, tuple struct path -> struct instance, etc
                    match rust_type {
                        RustType2::ImplTrait(_) => {
                            dbg!(expr_call);
                            todo!()
                        }
                        RustType2::TypeParam(_) => {
                            // TODO if type param is resolved it could be a fn or whatever so need to call this match recursively/as a fn
                            // otherwise need return the unresolved type but also note that it has been called(!) so could be the return type of some fn yet to be known, a tuple struct instance, and enum instance, etc. Though this seems like a rarer case and I'm not sure it is even possbile/aloud to wait until after calling a type param to resolved
                            dbg!(rust_type);
                            todo!()
                        }
                        RustType2::Fn(_item_type_params, _type_params, fn_info) => {
                            // let name = match name {
                            // let fn_info = match rust_type_fn_type {
                            //     RustTypeFnType::Standalone(fn_info) => global_data
                            //         .lookup_fn_info_known_module(&module_path, &scope_id, &name),
                            //     RustTypeFnType::AssociatedFn(item_name, fn_name) => {
                            //         let _item_type_params = item_type_params.unwrap();
                            //         let item_definition = global_data
                            //             .lookup_item_def_known_module_assert_not_func2(
                            //                 &module_path,
                            //                 &scope_id,
                            //                 &item_name.clone(),
                            //             );
                            //         let impl_method = global_data
                            //             .lookup_impl_item_item2(
                            //                 &item_definition,
                            //                 // TODO IMPORTANT not populating turbofish correctly
                            //                 &RustPathSegment {
                            //                     ident: fn_name.clone(),
                            //                     turbofish: Vec::new(),
                            //                 },
                            //             )
                            //             .unwrap();
                            //         match impl_method.item {
                            //             RustImplItemItemNoJs::Fn(_, fn_info) => fn_info,
                            //             RustImplItemItemNoJs::Const => todo!(),
                            //         }
                            //     }
                            // };

                            let fn_inputs = fn_info
                                .inputs_types
                                .iter()
                                .map(|(_, _, _, rust_type)| rust_type.clone())
                                .collect::<Vec<_>>();

                            // TODO need to break up this whole fn and avoid calling parse_args a second time here
                            let (new_args_js_expr, args_rust_types) = parse_args(
                                expr_call,
                                Some(&fn_inputs),
                                global_data,
                                current_module,
                            );
                            args_js_expr = new_args_js_expr;

                            // Look to see if any of the input types are type params replace with concrete type from argument
                            // TODO IMPORTANT for an associated fn, attempt_to_resolve_type_params_using_arg_types will not take into account generics defined on the *item* which might be concretised by the args and used in the return type
                            let new_type_params = fn_info
                                .attempt_to_resolve_type_params_using_arg_types(&args_rust_types);

                            // Resolve return type
                            // RustType::Fn returns a different type when called (which is obviously what is happening given we are handling expr_call), and also might be nested, ie inside other types that take generics eg Some(fn_returns_i32()) => RustType::Option(RustType::i32), so need to resolve this
                            fn get_fn_type_returns(
                                return_type: RustType,
                                current_type_params: &[RustTypeParam2],
                                global_data: &GlobalData,
                            ) -> RustType2 {
                                match &return_type {
                                    RustType::Unknown => todo!(),
                                    RustType::Todo => todo!(),
                                    RustType::TypeParam(rust_type_param) => {
                                        // Assert that a type param return type on a FnInfo should always be unresolved
                                        match rust_type_param.type_ {
                                            RustTypeParamValue::Unresolved => {}
                                            RustTypeParamValue::RustType(_) => panic!(),
                                        }

                                        // I think we can do this because it is impossible for a fn to return a type param that is not defined on the fn (FnInfo). Methods/structs/impls might be a bit more complicated
                                        let maybe_resolved_type = current_type_params
                                            .iter()
                                            .find(|tp| tp.name == rust_type_param.name)
                                            .unwrap()
                                            .clone();
                                        match maybe_resolved_type.type_ {
                                            RustTypeParamValue2::Unresolved => {
                                                return_type.into_rust_type2(global_data)
                                            }
                                            RustTypeParamValue2::RustType(resolved_type) => {
                                                *resolved_type
                                            }
                                        }
                                    }
                                    RustType::Option(_rust_type) => todo!(),
                                    RustType::Result(_) => todo!(),
                                    // RustType::StructOrEnum(_, _, _) => todo!(),
                                    RustType::Vec(_) => todo!(),
                                    RustType::Array(_) => todo!(),
                                    RustType::Tuple(_) => todo!(),
                                    RustType::UserType(_, _) => todo!(),
                                    // RustType::MutRef(_) => todo!(),
                                    RustType::Ref(_) => todo!(),
                                    RustType::Fn(_, _, _, _, _) => todo!(),
                                    _ => return_type.into_rust_type2(global_data),
                                }
                            }
                            get_fn_type_returns(
                                fn_info.return_type.clone(),
                                &new_type_params,
                                global_data,
                            )
                        }
                        RustType2::Closure(_input_types, return_type) => {
                            //
                            *return_type
                        }
                        RustType2::Vec(_) => todo!(),
                        RustType2::FnVanish => {
                            //
                            RustType2::FnVanish
                        }
                        _ => {
                            dbg!(partial_rust_type);
                            panic!("type can't be called")
                        }
                    }
                }
            };

            // fn get_fn_type_returns(rust_type: RustType) -> RustType {
            //     match rust_type {
            //         RustType::Fn(_type_params, module_path, fn_name) => {
            //             get_fn_type_returns(*return_type)
            //         }
            //         RustType::Option(rust_type) => {
            //             RustType::Option(Box::new(get_fn_type_returns(*rust_type)))
            //         }
            //         _ => rust_type,
            //     }
            // }
            match partial_rust_type {
                PartialRustType::StructIdent(_, item_def) => {
                    // let js_path = match expr {
                    //     JsExpr::Path(path) => path,
                    //     _ => todo!(),
                    // };
                    // (JsExpr::New(js_path, args_js_expr.clone()), rust_type)
                    (
                        JsExpr::New(
                            PathIdent::Single(item_def.js_name.clone()),
                            args_js_expr.clone(),
                        ),
                        rust_type,
                    )
                }
                PartialRustType::RustType(RustType2::FnVanish, _, _) => {
                    // TODO for now we are assuming we are dealing with Box::new() so the args must be len=1
                    assert_eq!(args_js_expr.len(), 1);
                    (args_js_expr.remove(0), args_rust_types.remove(0))
                }
                PartialRustType::EnumVariantIdent(_, item_def, variant_name) => {
                    // TODO properly check for prelude Option and avoid shadowed idents
                    if item_def.ident == "Option" && variant_name == "Some" {
                        (args_js_expr.remove(0), rust_type)
                    } else {
                        (
                            JsExpr::FnCall(Box::new(expr), args_js_expr.clone()),
                            rust_type,
                        )
                    }
                }
                PartialRustType::RustType(rust_type2, _, _) => {
                    match rust_type2 {
                        // TODO fn_def should be a ref to the actual definition so we can can just compare pointers and don't have to check it is not a user shadowed var etc.
                        RustType2::Fn(_, _, fn_def) if fn_def.ident == "document" => {
                            (
                                JsExpr::Var("document".to_string()),
                                // TODO would it would be nicer/cleaner to just directly get `Document` here? Using fn_def.return_type seems miore generalisable?
                                fn_def.return_type.clone().into_rust_type2(global_data),
                            )
                        }
                        // TODO check this isn't a user fn
                        // TODO use attributes in source code instead then we don't have to check
                        RustType2::Fn(_, _, fn_def) if fn_def.ident == "json_parse" => (
                            JsExpr::FnCall(
                                Box::new(JsExpr::Path(PathIdent::PathTwo([
                                    Ident::Str("JSON"),
                                    Ident::Str("parse"),
                                ]))),
                                args_js_expr.clone(),
                            ),
                            rust_type,
                        ),
                        _ => (
                            JsExpr::FnCall(Box::new(expr), args_js_expr.clone()),
                            rust_type,
                        ),
                    }
                }
            }
        }

        _ => (
            JsExpr::FnCall(
                Box::new(handle_expr(&expr_call.func, global_data, current_module).0),
                args_js_expr,
            ),
            RustType2::Todo,
        ),
    }
}

pub fn handle_expr_path(
    expr_path: &RustExprPath,
    global_data: &mut GlobalData,
    current_module: &[String],
    // is_call: bool,
) -> (JsExpr, PartialRustType) {
    handle_expr_path_inner(&expr_path.path, global_data, current_module)
}

/// is_call: is this path being called eg foo() or Foo()
///
/// Rather than return RustType we return PartialRustType to handle the fact that a path might not be a full expression which evaluates to a type, it might only be part of an expression eg the `TupleStruct` in `TupleStruct()` or `Enum::Variant` in `Enum::Variant(5)`
///
fn handle_expr_path_inner(
    // expr_path: &ExprPath,
    // Use Path in place of ExprPath since eg ExprStruct has a Path that needs handling, but not a ExprPath
    expr_path: &syn::Path,
    global_data: &mut GlobalData,
    current_module: &[String],
    // is_call: bool,
) -> (JsExpr, PartialRustType) {
    let span = debug_span!("handle_expr_path", expr_path = ?quote! { #expr_path }.to_string());
    let _guard = span.enter();

    // TODO I think a lot of this messing around with CamelCase and being hard to fix is because we should be storing the namespaced item names as Vecs intead of foo__Bar, until they are rendered to JS

    // IMPORTANT TODO
    // What is all this doing? We just got the module path to an item using `get_path()` so we should be looking up the definition of found item and then if there is remaining segments in the expr_path after then item then determine if these are an enum variant or associated fn
    // What does get_path() return if the expr_path is just a scoped variable?
    let segs_copy = expr_path
        .segments
        .iter()
        .map(|seg| RustPathSegment2 {
            ident: seg.ident.to_string(),
            turbofish: Vec::new(),
        })
        .collect::<Vec<_>>();

    // NOTE if segs_copy len = 1 then we don't need to resolve the path
    // TODO clean this up
    // get_path doesn't handle vars, it just resolves paths to *items*

    // dbg!("handle_expr_path_inner");
    // println!("{}", quote! { #expr_path });
    // dbg!(&current_module);
    // dbg!(&global_data.scope_id_as_option());

    // dbg!(&current_module);
    // dbg!(&segs_copy);
    // dbg!(&global_data.item_refs);
    // dbg!(&global_data.item_defs);
    let (segs_copy_module_path, segs_copy_item_path, segs_copy_is_scoped, segs_copy_index) =
        resolve_path(
            true,
            // By definition handle_expr_path is always handling *expressions* so want to look for scoped vars
            true,
            true,
            true,
            segs_copy,
            &global_data.crates,
            &global_data.item_defs,
            current_module,
            current_module,
            &global_data.scopes,
        );
    // dbg!(&segs_copy_module_path);
    // dbg!(&segs_copy_item_path);
    // dbg!(&segs_copy_is_scoped);
    // dbg!(&segs_copy_index);

    assert!(segs_copy_item_path.len() <= 2);

    let item_path_seg = &segs_copy_item_path[0];

    // Split out item and any sub path eg for an enum variant, associated fn, etc
    // Different cases we are handling:
    // segs len = 1
    // 1. A variable
    // 2. A const
    // 3. A fn ident ie `my_function` (note the fn itself, not it's return type)
    // 4. A struct ident ie `my_struct` (ie PartialRustType::StructIdent, not an actual instance, though I think it could be if it is a unit struct)
    // 5. TODO len = 1 enums like Some, None, etc
    // segs len = 2
    // 1. Associated fn or const
    // 2. Enum variant (an actual instance if the variant takes no args, otherwise a PartialRustType::EnumVariantIdent)

    // dbg!(&segs_copy_module_path);
    // dbg!(&segs_copy_item_scope);
    // dbg!(&segs_copy_item_path);

    // NOTE for a var with prelude type the segs_copy_module_path will not be PRELUDE_MODULE_PATH, it will be the scope in which the var is instantiated
    let (partial_rust_type, mut js_segs_item_path) =
        if segs_copy_module_path == [RUST_PRELUDE_MODULE_PATH] {
            // NOTE I believe that for a "prelude_special_case" type we either must have a path to the actual prelude type (see else branch) or a variable which is a prelude type, no other possibilities eg a scoped prelude type
            if segs_copy_is_scoped {
                // Look for var
                // NOTE this will only catch vars defined *in* the PRELUDE_MODULE_PATH module. Vars that are defined outside of the module but have a prelude type will not have PRELUDE_MODULE_PATH as their module path, it will be whatever module they were defined in
                assert_eq!(segs_copy_item_path.len(), 1);
                let path = segs_copy_item_path.first().unwrap();
                // dbg!(&segs_copy_item_path);

                // NOTE prelude type definitions cannot be scoped so we only need to look for vars
                // TODO look through all transparent scopes
                let var = global_data
                    .scopes
                    .iter()
                    .rev()
                    .find_map(|s| s.variables.iter().find(|v| v.name == path.ident))
                    .unwrap();

                (
                    PartialRustType::RustType(var.type_.clone(), var.mut_, true),
                    vec![Ident::String(path.ident.clone())],
                )
            } else {
                let path_idents = segs_copy_item_path
                    .iter()
                    .map(|seg| seg.ident.as_str())
                    .collect::<Vec<_>>();
                // TODO need to know whether we have mut var like `let mut foo = Box::new;`???
                match path_idents[..] {
                    ["Box", "new"] => (
                        PartialRustType::RustType(RustType2::FnVanish, false, false),
                        segs_copy_item_path
                            .clone()
                            .into_iter()
                            .map(|seg| Ident::String(seg.ident))
                            .collect::<Vec<_>>(),
                    ),
                    ["Some"] => {
                        // Is it a problem using PartialRustType::EnumVariantIdent rather than a specific PartialRustType::OptionVariantIdent?
                        assert_eq!(segs_copy_module_path, [RUST_PRELUDE_MODULE_PATH]);
                        (
                            PartialRustType::EnumVariantIdent(
                                // TODO handle turbofish
                                Vec::new(),
                                global_data.get_prelude_item_def("Option"),
                                "Some".to_string(),
                            ),
                            segs_copy_item_path
                                .clone()
                                .into_iter()
                                .map(|seg| Ident::String(seg.ident))
                                .collect::<Vec<_>>(),
                        )
                    }
                    ["None"] => {
                        let item_def = global_data.get_prelude_item_def("Option");
                        assert_eq!(item_def.generics.len(), 1);
                        let rust_type_type_param = match &item_def.struct_or_enum_info {
                            StructEnumUniqueInfo2::Struct(_) => todo!(),
                            StructEnumUniqueInfo2::Enum(enum_def_info) => {
                                // Some member has a RustType::TypeParam input which is what we want
                                // TODO yes but surely it will always be RustType::TypeParam(T)??? Is this simply for getting the name of the generic? Maybe we should assert this?
                                let some_member = enum_def_info
                                    .members
                                    .iter()
                                    .find(|m| m.ident == "Some")
                                    .unwrap();
                                assert_eq!(some_member.inputs.len(), 1);
                                match some_member.inputs.first().unwrap() {
                                    EnumVariantInputsInfo::Named { .. } => todo!(),
                                    EnumVariantInputsInfo::Unnamed(rust_type) => {
                                        rust_type.clone().into_rust_type2(global_data)
                                    }
                                }
                            }
                        };
                        let rust_type_param = match rust_type_type_param {
                            RustType2::TypeParam(rust_type_param) => rust_type_param,
                            _ => panic!(),
                        };
                        let option = RustType2::Option(rust_type_param);
                        (
                            PartialRustType::RustType(option, false, false),
                            segs_copy_item_path
                                .clone()
                                .into_iter()
                                .map(|seg| Ident::String(seg.ident))
                                .collect::<Vec<_>>(),
                        )
                    }
                    _ => todo!(),
                }
            }
        } else if segs_copy_item_path.len() == 1 {
            // TODO IMPORTANT needs to look/iterate through the static scopes, and var scope in unison, because they can shadow each other.
            // Is path a variable?

            let scoped_partial_rust_type = global_data.scopes.iter().rev().find_map(|scope| {
                let def = scope.items.iter().find_map(|index| {
                    let item_def = &global_data.item_defs[*index];
                    (item_def.ident() == item_path_seg.ident).then_some(item_def)
                });

                let var = scope
                    .variables
                    .iter()
                    .find(|var| var.name == item_path_seg.ident);

                if segs_copy_is_scoped && (var.is_some() || def.is_some()) {
                    Some(found_item_to_partial_rust_type(
                        item_path_seg,
                        var,
                        def,
                        global_data,
                    ))
                } else {
                    None
                }
            });

            // TODO handle user defined len=1 enums like Some(5), None, etc
            let (final_partial_rust_type, path_ident) =
                if let Some(scoped_partial_rust_type) = scoped_partial_rust_type {
                    scoped_partial_rust_type
                } else {
                    // We don't have a scoped match so path must be a module level definiton
                    let item_module = global_data.get_module(&segs_copy_module_path);
                    let item_def = item_module
                        .items
                        .iter()
                        .find_map(|item_ref| match item_ref {
                            ItemRef::StructOrEnum(index) => {
                                let item = &global_data.item_defs[*index];
                                (item.ident() == item_path_seg.ident).then_some(item)
                            }
                            ItemRef::Fn(index) => {
                                let item = &global_data.item_defs[*index];
                                (item.ident() == item_path_seg.ident).then_some(item)
                            }
                            ItemRef::Const(index) => {
                                let item = &global_data.item_defs[*index];
                                (item.ident() == item_path_seg.ident).then_some(item)
                            }
                            _ => None,
                        });

                    found_item_to_partial_rust_type(item_path_seg, None, item_def, global_data)
                };
            (final_partial_rust_type, vec![path_ident])
        } else if segs_copy_item_path.len() == 2 {
            // NOTE path must start with a struct or enum if item part of the path is length = 2

            // NOTE when specifying type params for enum instantiation with turbofish, we can use *either* `Enum::<usize>::Variant(5)` *or* `Enum::Variant::<usize>(5)`, however for associated fns we must use only use `Struct::<usize>::associated_fn()` for type params of the struct and `Struct::associated_fn::<usize>()` only for type params defined on the method itself.

            let sub_path = &segs_copy_item_path[1];
            // ie:
            // Struct/Enum::associated_fn
            // Struct/Enum::associated_const
            // Enum::Variant
            // Enum::Variant ()
            // Enum::Variant {}

            let item = &global_data.item_defs[segs_copy_index.unwrap()];
            let item_def = match item {
                ItemDefRc::StructEnum(item_def) => item_def,
                _ => todo!(),
            };

            // dbg!(&item_def);

            // If turbofish exists on item path segment then use that for type params, otherwise use the unresolved params defined on the item definition
            let item_generics = if !item_path_seg.turbofish.is_empty() {
                item_path_seg
                    .turbofish
                    .iter()
                    .enumerate()
                    .map(|(i, g)| RustTypeParam2 {
                        name: item_def.generics[i].clone(),
                        type_: RustTypeParamValue2::RustType(Box::new(g.clone())),
                    })
                    .collect::<Vec<_>>()
            } else {
                // NOTE for now we are assuming turbofish must exist for generic items, until we implement a solution for getting type params that are resolved later in the code
                assert!(item_def.generics.is_empty());
                item_def
                    .generics
                    .iter()
                    .map(|g| RustTypeParam2 {
                        name: g.clone(),
                        type_: RustTypeParamValue2::Unresolved,
                    })
                    .collect::<Vec<_>>()
            };

            // TODO don't like returning an Option here, should probably follow how rust does which I believe is to see if it is an enum variant first else it must be an associated fn, else panic
            let impl_method = global_data.lookup_associated_fn(
                &item_generics,
                &segs_copy_module_path,
                &None,
                sub_path,
                &item_path_seg.ident,
                item_def,
            );
            // dbg!(&impl_method);
            let impl_method =
                impl_method.map(|rust_type| PartialRustType::RustType(rust_type, false, false));

            let enum_variant = match &item_def.struct_or_enum_info {
                // Item is struct so we need to look up associated fn
                StructEnumUniqueInfo2::Struct(_struct_definition_info) => None,
                StructEnumUniqueInfo2::Enum(enum_definition_info) => {
                    // Check if we have a variant of the enum
                    let enum_variant = enum_definition_info
                        .members
                        .iter()
                        .find(|member| member.ident == sub_path.ident);

                    let enum_variant_generics = if !sub_path.turbofish.is_empty() {
                        sub_path
                            .turbofish
                            .iter()
                            .enumerate()
                            .map(|(i, g)| RustTypeParam2 {
                                name: item_def.generics[i].clone(),
                                type_: RustTypeParamValue2::RustType(Box::new(g.clone())),
                            })
                            .collect::<Vec<_>>()
                    } else {
                        item_def
                            .generics
                            .iter()
                            .map(|g| RustTypeParam2 {
                                name: g.clone(),
                                type_: RustTypeParamValue2::Unresolved,
                            })
                            .collect::<Vec<_>>()
                    };

                    let mut enum_generics = Vec::new();
                    // An enum variant instantiation cannot have turbofish on both the enum and the variant
                    if !item_path_seg.turbofish.is_empty() {
                        assert!(!sub_path.turbofish.is_empty());
                        enum_generics = item_generics;
                    }
                    if !sub_path.turbofish.is_empty() {
                        assert!(!item_path_seg.turbofish.is_empty());
                        enum_generics = enum_variant_generics;
                    }
                    // NOTE for now we are assuming turbofish must exist for generic items, until we implement a solution for getting type params that are resolved later in the code
                    if !item_def.generics.is_empty() {
                        assert!(!enum_generics.is_empty());
                    }

                    enum_variant.map(|enum_variant| {
                        if enum_variant.inputs.is_empty() {
                            PartialRustType::RustType(
                                RustType2::StructOrEnum(enum_generics, item_def.clone()),
                                false,
                                false,
                            )
                        } else {
                            PartialRustType::EnumVariantIdent(
                                enum_generics,
                                item_def.clone(),
                                sub_path.ident.clone(),
                            )
                        }
                    })
                }
            };
            // If you have an enum variant and associated fn with the same name, the code will compile, but if you try to access the fn you will just get the variant instead
            let mut ident_path = segs_copy_item_path
                .clone()
                .into_iter()
                .map(|seg| Ident::String(seg.ident))
                .collect::<Vec<_>>();
            // dbg!(&item_def);
            ident_path[0] = item_def.js_name.clone();
            if let Some(enum_variant) = enum_variant {
                (enum_variant, ident_path)
            } else if let Some(impl_method) = impl_method {
                (impl_method, ident_path)
            } else {
                panic!()
            }
        } else {
            // Not sure how an item can have a path with len 0 or greater than 2, panic if it happens so I can see this case
            todo!()
        };

    // TODO methods and associated fns can have their own generics (separate to the struct they are defined on) and thus can have their own turbofish, also associated fns can be assigned to vars so we need to ensure we are storing any turbofish-resolved types on the returned RustType::Fn
    // Get any generics specified on the path
    // let generics = expr_path.path.segments.ite

    // TODO Separate module path and item path ie `root_module::child_module` and `MyEnum::MyVariant`
    // TODO Assume first element in segs is an item (so not handling vars here), and the item is scoped
    // Algorithm:
    //

    // A mut var with a JS primative type will have a wrapper like RustInteger and should therefore be copied when it is used, unless:
    // 1. It is a &mut eg for: `let foo = &mut 5; fn_takes_mut_ref(foo);` we want to preserve the `RustInteger` wrapper.
    // 2. We are taking a &mut, in which case the var will already be mut and so wrapped in `RustInteger` which we want to preserve.
    // For non mut vars JS will do this anyway since JS primatives are copied

    // If we are taking &mut, we must have a var and it must be `mut`
    // if is_having_mut_ref_taken && !is_mut_var {
    //     panic!("a mutable reference can only be taken of a mutable var");
    // }

    // dbg!(&partial_rust_type);
    // let is_mut_ref_js_primative = match &partial_rust_type {
    //     PartialRustType::RustType(RustType::MutRef(rust_type)) => rust_type.is_js_primative(),
    //     _ => false,
    // };

    // Make JS path
    // segs_copy_module_path, segs_copy_item_path, is_scoped
    // Lookup module path to find what it's deduplicated name is
    // Check whether it is not globally unique and so has been namespaced
    // dbg!(&segs_copy_item_path);

    // dbg!(&segs_copy_item_path);
    // dbg!(&partial_rust_type);

    if segs_copy_module_path == ["web_prelude"] && js_segs_item_path[0] == "Document" {
        js_segs_item_path[0] = Ident::Str("document");
    }

    // TODO why are we converting to JS ident here? JS AST should just store namespace ident and do the join("__") at `.to_string()` time
    // TODO Surely this should be `.is_none()`?
    // if segs_copy_item_scope.is_some() {
    // if segs_copy_item_scope.is_none() {
    //     if let Some(dup) = global_data.duplicates.iter().find(|dup| {
    //         js_segs_item_path[0] == dup.name && dup.original_module_path == segs_copy_module_path
    //     }) {
    //         js_segs_item_path[0] = Ident::Deduped(dup.namespace.clone());
    //     }
    // } else if js_segs_item_path[0] == "self" {
    //     js_segs_item_path[0] = Ident::Str("this");
    // }
    if js_segs_item_path[0] == "self" {
        js_segs_item_path[0] = Ident::Str("this");
    }

    // dbg!(&js_segs_path);
    // let segs = segs_copy.iter()
    // TODO all this logic could be cleaned up and/or made clearer
    // let final_expr = if segs_copy_module_path == [PRELUDE_MODULE_PATH] {
    //     JsExpr::Null
    // } else if is_mut_ref_js_primative || is_having_mut_ref_taken || !is_mut_var {
    //     // dbg!("no inner");
    //     // dbg!(is_mut_ref_js_primative);
    //     // dbg!(is_having_mut_ref_taken);
    //     // dbg!(!is_mut_var);
    //     JsExpr::Path(PathIdent::Path(js_segs_path))

    //     // TODO how/should we take into account scope id, in the same way we do when handling the `PartialRustType`
    // } else {
    //     match &partial_rust_type {
    //         PartialRustType::StructIdent(_, _, _, _) => todo!(),
    //         PartialRustType::EnumVariantIdent(_, _, _, _, _) => todo!(),
    //         PartialRustType::RustType(rust_type) => {
    //             if rust_type.is_js_primative() {
    //                 // dbg!("inner");
    //                 JsExpr::Field(
    //                     Box::new(JsExpr::Path(PathIdent::Path(js_segs_path))),
    //                     Ident::Str("inner"),
    //                 )
    //             } else {
    //                 // dbg!("no inner2");
    //                 // TODO Need to .copy() for non-primative types, and check they are `Copy` else panic because they would need to be cloned?
    //                 JsExpr::Path(PathIdent::Path(js_segs_path))
    //             }
    //         }
    //     }
    // };
    // dbg!(&js_segs_path);
    let final_expr = if segs_copy_module_path == [RUST_PRELUDE_MODULE_PATH] {
        JsExpr::Null
    } else {
        // match (
        //     is_mut_ref_js_primative,
        //     is_having_mut_ref_taken,
        //     !is_mut_var,
        //     &partial_rust_type,
        // ) {
        //     (false, false, true, PartialRustType::RustType(rust_type)) => {
        //         if false {
        //             // dbg!("inner");
        //             JsExpr::Field(
        //                 Box::new(JsExpr::Path(PathIdent::Path(js_segs_path))),
        //                 Ident::Str("inner"),
        //             )
        //         } else {
        //             // dbg!("no inner2");
        //             // TODO Need to .copy() for non-primative types, and check they are `Copy` else panic because they would need to be cloned?
        //             JsExpr::Path(PathIdent::Path(js_segs_path))
        //         }
        //     }
        //     (true, _, _, _) => JsExpr::Path(PathIdent::Path(js_segs_path)),
        //     (_, true, _, _) => JsExpr::Path(PathIdent::Path(js_segs_path)),
        //     (_, _, true, _) => JsExpr::Path(PathIdent::Path(js_segs_path)),
        //     (_, _, _, PartialRustType::StructIdent(_, _, _, _)) => todo!(),
        //     (_, _, _, PartialRustType::EnumVariantIdent(_, _, _, _, _)) => todo!(),
        //     (_, _, _, PartialRustType::RustType(rust_type)) => {
        //         if rust_type.is_js_primative() {
        //             // dbg!("inner");
        //             JsExpr::Field(
        //                 Box::new(JsExpr::Path(PathIdent::Path(js_segs_path))),
        //                 Ident::Str("inner"),
        //             )
        //         } else {
        //             // dbg!("no inner2");
        //             // TODO Need to .copy() for non-primative types, and check they are `Copy` else panic because they would need to be cloned?
        //             JsExpr::Path(PathIdent::Path(js_segs_path))
        //         }
        //     }
        // }

        JsExpr::Path(PathIdent::Path(js_segs_item_path))

        // dbg!("no inner");
        // dbg!(is_mut_ref_js_primative);
        // dbg!(is_having_mut_ref_taken);
        // dbg!(!is_mut_var);

        // TODO how/should we take into account scope id, in the same way we do when handling the `PartialRustType`
    };
    (final_expr, partial_rust_type)
}

/// Get match pattern ident to be used as rhs of if conditions like `myData.id === MyEnum.fooId`, and start a body stmts Vec to with any pattern arg destructuring that might be necessary
///
/// (rhs, assignments/destructuring at start of body Vec)
fn handle_match_pat(
    arm_pat: &Pat,
    expr_match: &RustExprMatch,
    global_data: &mut GlobalData,
    current_module: &[String],
    match_condition_type: &RustType2,
) -> (Vec<String>, Vec<JsStmt>, Vec<ScopedVar>) {
    match arm_pat {
        Pat::Const(_) => todo!(),
        Pat::Ident(pat_ident) => {
            let empty_vec: Vec<JsStmt> = Vec::new();
            let ident = pat_ident.ident.to_string();
            (vec![ident], empty_vec, Vec::new())
        }
        Pat::Lit(_) => todo!(),
        Pat::Macro(_) => todo!(),
        Pat::Or(_) => todo!(),
        Pat::Paren(_) => todo!(),
        Pat::Path(pat_path) => {
            let empty_vec: Vec<JsStmt> = Vec::new();
            (
                pat_path
                    .path
                    .segments
                    .iter()
                    .map(|seg| seg.ident.to_string())
                    .collect::<Vec<_>>(),
                empty_vec,
                Vec::new(),
            )
        }
        Pat::Range(_) => todo!(),
        Pat::Reference(_) => todo!(),
        Pat::Rest(_) => todo!(),
        Pat::Slice(_) => todo!(),
        Pat::Struct(pat_struct) => {
            let names = pat_struct
                .fields
                .iter()
                .map(|field| match &field.member {
                    Member::Named(ident) => DestructureValue::KeyName(Ident::Syn(ident.clone())),
                    Member::Unnamed(_) => todo!(),
                })
                .collect::<Vec<_>>();

            // A struct pattern in a match arm always implies an enum? if so, look up enum to find types of struct patterns
            let enum_def_info = match match_condition_type {
                RustType2::StructOrEnum(_type_params, item_def) => {
                    match &item_def.struct_or_enum_info {
                        StructEnumUniqueInfo2::Struct(_) => todo!(),
                        StructEnumUniqueInfo2::Enum(enum_def_info) => enum_def_info,
                    }
                }
                _ => {
                    dbg!(match_condition_type);
                    dbg!(pat_struct);
                    todo!()
                }
            };
            let arm_field_defs = enum_def_info
                .members
                .iter()
                .find(|member| pat_struct.path.segments.last().unwrap().ident == member.ident)
                .unwrap();

            let scoped_vars = pat_struct
                .fields
                .iter()
                .map(|field| match &field.member {
                    Member::Named(ident) => {
                        let field_type = arm_field_defs
                            .inputs
                            .iter()
                            .find_map(|input| match input {
                                EnumVariantInputsInfo::Named {
                                    ident: field_def_ident,
                                    input_type,
                                } => (ident == field_def_ident).then_some(input_type),
                                EnumVariantInputsInfo::Unnamed(_) => todo!(),
                            })
                            .unwrap()
                            .clone();

                        ScopedVar {
                            name: ident.to_string(),
                            // TODO can't find the data for mutability on `field`
                            mut_: false,
                            // mut_ref: false,
                            type_: field_type.into_rust_type2(global_data),
                        }
                    }
                    Member::Unnamed(_) => todo!(),
                })
                .collect::<Vec<_>>();
            let stmt = JsStmt::Local(JsLocal {
                public: false,
                export: false,
                type_: LocalType::Var,
                lhs: LocalName::DestructureObject(DestructureObject(names)),
                value: JsExpr::Field(
                    Box::new(handle_expr(&expr_match.expr, global_data, current_module).0),
                    Ident::Str("data"),
                ),
            });
            let rhs = pat_struct
                .path
                .segments
                .iter()
                .map(|seg| seg.ident.to_string())
                .collect::<Vec<_>>();
            (rhs, vec![stmt], scoped_vars)
        }
        Pat::Tuple(_) => todo!(),
        Pat::TupleStruct(pat_tuple_struct) => {
            // let enum_info = match match_condition_type {
            //     RustType::StructOrEnum(type_params, module_path, name) => {
            //         let item_def = global_data.lookup_item_def_known_module_assert_not_func(module_path, name);
            //         match item_def.struct_or_enum_info {
            //             StructOrEnumDefitionInfo::Struct(_) => todo!(),
            //             StructOrEnumDefitionInfo::Enum(enum_def_info) => {
            //                 enum_def_info.members.iter().find_map(|enum_var_info| enum_var_info.ident == )
            //             },
            //         }
            //     },
            //     _ => todo!(),
            // };
            let mut names = pat_tuple_struct
                .elems
                .iter()
                // TODO why is RustType::I32 hardcoded here?
                .map(|elem| handle_pat(elem, global_data, RustType2::I32))
                .collect::<Vec<_>>();

            // A TupleStruct pattern in a match arm always implies an enum? if so, look up enum to find types of struct patterns
            fn get_item_def_from_rust_type(
                match_condition_type: &RustType2,
                global_data: &GlobalData,
            ) -> (Rc<StructEnumDef>, bool) {
                match match_condition_type {
                    RustType2::StructOrEnum(_type_params, item_def) => (item_def.clone(), false),
                    RustType2::Option(_) => (global_data.get_prelude_item_def("Option"), true),
                    _ => {
                        dbg!(match_condition_type);
                        todo!()
                    }
                }
            }
            let (item_def, is_option) =
                get_item_def_from_rust_type(match_condition_type, global_data);
            let enum_def_info = match &item_def.struct_or_enum_info {
                StructEnumUniqueInfo2::Struct(_) => todo!(),
                StructEnumUniqueInfo2::Enum(enum_def_info) => enum_def_info,
            };
            let variant_def = enum_def_info
                .members
                .iter()
                .find(|member| pat_tuple_struct.path.segments.last().unwrap().ident == member.ident)
                .unwrap();

            let scoped_vars = pat_tuple_struct
                .elems
                .iter()
                .enumerate()
                .map(|(i, elem)| {
                    let name = match elem {
                        Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
                        // TODO handle len > 1
                        // Pat::Path(pat_path) => {
                        //     pat_path.path.segments.first().unwrap().ident.to_string()
                        // }
                        _ => todo!(),
                    };

                    // Get input from definition based on it's position
                    // TODO what about use of .. ??
                    let field_type = match &variant_def.inputs[i] {
                        EnumVariantInputsInfo::Named { .. } => todo!(),
                        EnumVariantInputsInfo::Unnamed(input_type) => input_type.clone(),
                    };

                    // If field type is a type param, see if it exists in parent/enum type and if it has been resolved then use resolved type
                    // TODO do this for Pat::Struct also
                    let field_type = match field_type {
                        RustType::TypeParam(rust_type_param) => {
                            fn type_to_type(
                                match_condition_type: &RustType2,
                                _rust_type_param: RustTypeParam,
                                _global_data: &GlobalData,
                            ) -> RustType2 {
                                match match_condition_type {
                                    // TODO should just mutate rust_type_param rather than create a new one
                                    RustType2::Option(type_param) => {
                                        // TODO don't assume match_condition_type is resolved, maybe it is resovled by turbofish?
                                        RustType2::TypeParam(type_param.clone())
                                    }
                                    RustType2::StructOrEnum(type_params, item_def) => {
                                        dbg!(type_params);
                                        dbg!(item_def);
                                        // dbg!(pat_tuple_struct);
                                        todo!()
                                    }
                                    _ => {
                                        dbg!(match_condition_type);
                                        // dbg!(pat_tuple_struct);
                                        todo!()
                                    }
                                }
                            }
                            type_to_type(match_condition_type, rust_type_param, global_data)
                        }
                        _ => field_type.into_rust_type2(global_data),
                    };

                    ScopedVar {
                        name,
                        mut_: false,
                        // mut_ref: false,
                        type_: field_type,
                    }
                })
                .collect::<Vec<_>>();

            let stmt = if is_option {
                assert_eq!(names.len(), 1);
                JsStmt::Local(JsLocal {
                    public: false,
                    export: false,
                    type_: LocalType::Let,
                    lhs: names.remove(0),
                    value: handle_expr(&expr_match.expr, global_data, current_module).0,
                })
            } else {
                JsStmt::Local(JsLocal {
                    public: false,
                    export: false,
                    type_: LocalType::Var,
                    lhs: LocalName::DestructureArray(names),
                    value: JsExpr::Field(
                        Box::new(handle_expr(&expr_match.expr, global_data, current_module).0),
                        Ident::Str("data"),
                    ),
                })
            };
            (
                pat_tuple_struct
                    .path
                    .segments
                    .iter()
                    .map(|seg| seg.ident.to_string())
                    .collect::<Vec<_>>(),
                vec![stmt],
                scoped_vars,
            )
        }
        Pat::Type(_) => todo!(),
        Pat::Verbatim(_) => todo!(),
        Pat::Wild(_) => todo!(),
        _ => todo!(),
    }
}

pub fn handle_expr_match(
    expr_match: &RustExprMatch,
    is_returned: bool,
    global_data: &mut GlobalData,
    current_module: &[String],
) -> (JsExpr, RustType2) {
    // (assignment, condition, succeed, fail)
    // TODO we need to know whether match result is being assigned to a var and therefore the if statement should be adding assignments to the end of each block
    // debug_span!("handle_expr_match", expr_match = ?quote! { #expr_match }.to_string());

    let (match_condition_expr, match_condition_type) =
        handle_expr(&expr_match.expr, global_data, current_module);

    fn handle_option_match(
        match_condition_expr: &JsExpr,
        match_condition_type: &RustType2,
        expr_match: &RustExprMatch,
        global_data: &mut GlobalData,
        current_module: &[String],
    ) -> Option<(JsExpr, RustType2)> {
        match match_condition_type {
            RustType2::Ref(_inner) => handle_option_match(
                match_condition_expr,
                match_condition_type,
                expr_match,
                global_data,
                current_module,
            ),
            RustType2::Option(_inner) => {
                assert_eq!(expr_match.arms.len(), 2);

                // make succeed
                let succeed_arm = expr_match
                    .arms
                    .iter()
                    .find(|arm| match &arm.pat {
                        Pat::TupleStruct(pat_tuple_struct) => {
                            let segs_len = pat_tuple_struct.path.segments.len();
                            assert!(segs_len == 1 || segs_len == 2);
                            pat_tuple_struct.path.segments.last().unwrap().ident == "Some"
                        }
                        _ => false,
                    })
                    .unwrap();
                let (mut cond_rhs, mut body_data_destructure, scoped_vars) = handle_match_pat(
                    &succeed_arm.pat,
                    expr_match,
                    global_data,
                    current_module,
                    match_condition_type,
                );

                // Need to take the path which will be eg [MyEnum, Baz], and convert to [MyEnum.bazId]
                let index = cond_rhs.len() - 1;
                cond_rhs[index] = format!("{}Id", cond_rhs[index].clone());
                global_data.scopes.push(GlobalDataScope {
                    variables: scoped_vars,
                    items: Vec::new(),
                    _look_in_outer_scope: true,
                    use_mappings: Vec::new(),
                });

                let (succeed_body_js_stmts, succeed_body_return_type) = match &*succeed_arm.body {
                    // Expr::Array(_) => [JsStmt::Raw("sdafasdf".to_string())].to_vec(),
                    // TODO not sure what this is for???
                    ExprRef::Array(_) => {
                        (vec![JsStmt::Raw("sdafasdf".to_string())], RustType2::Todo)
                    }
                    ExprRef::Block(expr_block) => {
                        dbg!("block");
                        // TODO should probably be using handle_body() or something for this
                        let (js_stmts, types_): (Vec<_>, Vec<_>) = expr_block
                            .stmts
                            .iter()
                            .flat_map(|stmt| handle_stmt(stmt, global_data, current_module))
                            .unzip();

                        let last_type = types_.last().unwrap().clone();

                        (js_stmts, last_type)
                    }
                    other_expr => {
                        let (js_expr, rust_type) =
                            handle_expr(other_expr, global_data, current_module);
                        (vec![JsStmt::Expr(js_expr, false)], rust_type)
                    }
                };

                body_data_destructure.extend(succeed_body_js_stmts);
                let mut succeed_body_js_stmts = body_data_destructure;

                // If last stmt is returned, add return stmt
                if !succeed_body_js_stmts.is_empty() {
                    let last_stmt = succeed_body_js_stmts.pop().unwrap();
                    let new_stmt = match last_stmt {
                        JsStmt::Expr(js_expr, has_semi) if !has_semi => {
                            JsStmt::Expr(JsExpr::Return(Box::new(js_expr.clone())), true)
                        }
                        _ => last_stmt,
                    };
                    succeed_body_js_stmts.push(new_stmt);
                }

                // pop match arm scope stuff
                global_data.scopes.pop();

                // body_data_destructure.extend(body_js_stmts.into_iter());
                // let succeed_body = body_data_destructure;

                // make fail
                let fail_arm = expr_match
                    .arms
                    .iter()
                    .find(|arm| match &arm.pat {
                        Pat::Ident(pat_ident) => pat_ident.ident == "None",
                        _ => false,
                    })
                    .unwrap();
                let (mut cond_rhs, _body_data_destructure, scoped_vars) = handle_match_pat(
                    &fail_arm.pat,
                    expr_match,
                    global_data,
                    current_module,
                    match_condition_type,
                );

                // Need to take the path which will be eg [MyEnum, Baz], and convert to [MyEnum.bazId]
                let index = cond_rhs.len() - 1;
                cond_rhs[index] = format!("{}_id", cond_rhs[index].clone());
                global_data.scopes.push(GlobalDataScope {
                    variables: scoped_vars,
                    items: Vec::new(),
                    _look_in_outer_scope: true,
                    use_mappings: Vec::new(),
                });

                let (mut fail_body_js_stmts, _fail_body_return_type) = match &*fail_arm.body {
                    // Expr::Array(_) => [JsStmt::Raw("sdafasdf".to_string())].to_vec(),
                    // TODO not sure what this is for???
                    ExprRef::Array(_) => {
                        (vec![JsStmt::Raw("sdafasdf".to_string())], RustType2::Todo)
                    }
                    ExprRef::Block(expr_block) => {
                        // TODO should probably be using handle_body() or something for this
                        let (js_stmts, types_): (Vec<_>, Vec<_>) = expr_block
                            .stmts
                            .iter()
                            .flat_map(|stmt| handle_stmt(stmt, global_data, current_module))
                            .unzip();

                        let last_type = types_.last().unwrap().clone();

                        (js_stmts, last_type)
                    }
                    other_expr => {
                        let (js_expr, rust_type) =
                            handle_expr(other_expr, global_data, current_module);
                        (vec![JsStmt::Expr(js_expr, false)], rust_type)
                    }
                };

                // If last stmt is returned, add return stmt
                if !fail_body_js_stmts.is_empty() {
                    let last_stmt = fail_body_js_stmts.pop().unwrap();
                    let new_stmt = match last_stmt {
                        JsStmt::Expr(js_expr, has_semi) if !has_semi => {
                            JsStmt::Expr(JsExpr::Return(Box::new(js_expr.clone())), true)
                        }
                        _ => last_stmt,
                    };
                    fail_body_js_stmts.push(new_stmt);
                }

                // pop match arm scope stuff
                global_data.scopes.pop();

                // body_data_destructure.extend(body_js_stmts.into_iter());
                // let fail_body = body_data_destructure;

                let if_stmts = JsStmt::Expr(
                    JsExpr::If(JsIf {
                        assignment: None,
                        declare_var: false,
                        condition: Box::new(JsExpr::Binary(
                            Box::new(match_condition_expr.clone()),
                            JsOp::NotEq,
                            Box::new(JsExpr::Null),
                        )),
                        succeed: succeed_body_js_stmts,
                        fail: Some(Box::new(JsExpr::Block(fail_body_js_stmts))),
                    }),
                    false,
                );
                Some((
                    JsExpr::FnCall(
                        Box::new(JsExpr::Paren(Box::new(JsExpr::ArrowFn(
                            false,
                            true,
                            Vec::new(),
                            vec![if_stmts],
                        )))),
                        Vec::new(),
                    ),
                    // TODO succeeed might be a todo!(), need to check and use fail otherwise, and write a test
                    succeed_body_return_type,
                    // TODO using fail because need to convert succeed type param into concrete type
                    // fail_body_return_type,
                ))
            }
            _ => None,
        }
    }
    if let Some((expr, rust_type)) = handle_option_match(
        &match_condition_expr,
        &match_condition_type,
        expr_match,
        global_data,
        current_module,
    ) {
        return (expr, rust_type);
    }

    // Fold match arms into if else statements
    // NOTE some arms might return never/unreachable (ie throw) so we can't just rely on eg getting the type from the first or last arm, we need to look through them all until we find a type which is not never/unreachable
    // TODO Why reveresed??
    // let (if_expr, rust_type) = expr_match.arms.iter().fold(
    let (if_expr, rust_type) = expr_match.arms.iter().rev().fold(
        (
            JsExpr::ThrowError("couldn't match enum variant".to_string()),
            // TODO we init with None because we want to make sure it is updated by the `match prev_body_return_type {}` logic, and don't want to set it to eg RustType::Todo because we want to panic and then update the code to eliminate these cases, not silently let them through.
            None,
        ),
        |acc, arm| {
            let (cond_rhs, mut body_data_destructure, scoped_vars) = handle_match_pat(
                &arm.pat,
                expr_match,
                global_data,
                current_module,
                &match_condition_type,
            );

            // if cond_rhs == ["Some"] || cond_rhs == ["None"] {
            //     cond_rhs.insert(0, "Option".to_string());
            // }

            assert_eq!(cond_rhs.len(), 2);

            // // Need to take the path which will be eg [MyEnum, Baz], and convert to [MyEnum.bazId]
            // let index = cond_rhs.len() - 1;
            // // dbg!(rhs);
            // // todo!();
            // // if rhs[0] == "Option" {
            // //     rhs = rhs[1..].to_vec();
            // // }
            // cond_rhs[index] = format!("{}_id", cond_rhs[index].clone());

            // Create new scope for match arm block
            // NOTE even if there is no curly braces it is still a scope
            global_data.scopes.push(GlobalDataScope {
                variables: scoped_vars,
                items: Vec::new(),
                _look_in_outer_scope: true,
                use_mappings: Vec::new(),
            });

            let (body_js_stmts, body_return_type) = match &*arm.body {
                // Expr::Array(_) => [JsStmt::Raw("sdafasdf".to_string())].to_vec(),
                // TODO not sure what this is for???
                ExprRef::Array(_) => (vec![JsStmt::Raw("sdafasdf".to_string())], RustType2::Todo),
                ExprRef::Block(expr_block) => {
                    // TODO should probably be using handle_body() or something for this
                    let (js_stmts, types_): (Vec<_>, Vec<_>) = expr_block
                        .stmts
                        .iter()
                        .flat_map(|stmt| handle_stmt(stmt, global_data, current_module))
                        .unzip();

                    let last_type = types_.last().unwrap().clone();

                    (js_stmts, last_type)
                }
                other_expr => {
                    let (js_expr, rust_type) = handle_expr(other_expr, global_data, current_module);
                    (vec![JsStmt::Expr(js_expr, false)], rust_type)
                }
            };

            // pop match arm scope stuff
            global_data.scopes.pop();

            body_data_destructure.extend(body_js_stmts);
            let body = body_data_destructure;

            let (prev_if_expr, prev_body_return_type) = acc;
            (
                JsExpr::If(JsIf {
                    assignment: is_returned
                        .then_some(LocalName::Single(Ident::Str("ifTempAssignment"))),
                    declare_var: is_returned,
                    condition: Box::new(JsExpr::Binary(
                        Box::new(JsExpr::Field(
                            Box::new(match_condition_expr.clone()),
                            Ident::Str("id"),
                        )),
                        JsOp::Eq,
                        Box::new(JsExpr::Field(
                            Box::new(JsExpr::Var(cond_rhs[0].clone())),
                            // Ident::String(format!("{}_id", cond_rhs[index].clone())),
                            Ident::NoConversion(format!("{}Id", cond_rhs[1].clone())),
                        )),
                    )),
                    succeed: body,
                    // TODO
                    fail: Some(Box::new(prev_if_expr)),
                }),
                match prev_body_return_type {
                    Some(prev_body_return_type) => Some(match prev_body_return_type {
                        RustType2::NotAllowed => todo!(),
                        RustType2::Unknown => todo!(),
                        RustType2::Todo => todo!(),
                        RustType2::Unit => todo!(),
                        RustType2::Never => body_return_type,
                        RustType2::ImplTrait(_) => todo!(),
                        RustType2::TypeParam(_) => todo!(),
                        RustType2::I32 => prev_body_return_type,
                        RustType2::F32 => prev_body_return_type,
                        RustType2::Bool => prev_body_return_type,
                        RustType2::String => prev_body_return_type,
                        RustType2::Option(_) => todo!(),
                        RustType2::Result(_) => todo!(),
                        RustType2::StructOrEnum(_, _) => prev_body_return_type,
                        RustType2::Vec(_) => prev_body_return_type,
                        RustType2::Array(_) => prev_body_return_type,
                        RustType2::Tuple(_) => prev_body_return_type,
                        RustType2::UserType(_, _) => todo!(),
                        RustType2::MutRef(_) => todo!(),
                        RustType2::Ref(_) => prev_body_return_type,
                        RustType2::Fn(_, _, _) => todo!(),
                        RustType2::Closure(_, _) => todo!(),
                        RustType2::FnVanish => todo!(),
                        RustType2::Box(_) => todo!(),
                        RustType2::Self_ => todo!(),
                    }),
                    None => Some(body_return_type),
                },
            )
        },
    );
    // for arm in &expr_match.arms {
    //     dbg!(arm);
    // }
    // todo!()
    (if_expr, rust_type.unwrap())
}

/// TODO This seems to be specifically for handling the case that `if segs_copy_item_path.len() == 1`. This fn and that branch that calls it needs cleaning up.
/// Assumes that exactly 1 of var, func, or item_def is Some() and the rest are None
/// -> (partial, is_mut)
fn found_item_to_partial_rust_type(
    item_path: &RustPathSegment2,
    var: Option<&ScopedVar>,
    item_def: Option<&ItemDefRc>,
    global_data: &GlobalData,
) -> (PartialRustType, Ident) {
    // debug!(item_path = ?item_path, var = ?var, func = ?func, item_def = ?item_def, module_path = ?module_path, "found_item_to_partial_rust_type");

    // IMPORTANT NOTE this fn only handles len=1 item paths which is useful for determing deduplicated idents because it allows us to disregard cases like associated fns.

    if let Some(var) = var {
        // This branch is obviously only possible for scoped paths since we can't have module level vars
        (
            PartialRustType::RustType(var.type_.clone(), var.mut_, true),
            Ident::String(item_path.ident.clone()),
        )
    } else if let Some(def) = item_def {
        match def {
            ItemDefRc::StructEnum(item_def) => {
                // If turbofish exists on item path segment then use that for type params, otherwise use the unresolved params defined on the item definition
                let item_generics = if !item_path.turbofish.is_empty() {
                    item_path
                        .turbofish
                        .iter()
                        .enumerate()
                        .map(|(i, g)| RustTypeParam2 {
                            name: item_def.generics[i].clone(),
                            type_: RustTypeParamValue2::RustType(Box::new(g.clone())),
                        })
                        .collect::<Vec<_>>()
                } else {
                    item_def
                        .generics
                        .iter()
                        .map(|g| RustTypeParam2 {
                            name: g.clone(),
                            type_: RustTypeParamValue2::Unresolved,
                        })
                        .collect::<Vec<_>>()
                };
                match &item_def.struct_or_enum_info {
                    StructEnumUniqueInfo2::Struct(_struct_definition_info) => {
                        // So we are assuming that *all* cases where we have an Expr::Path and the final segment is a struct ident, it must be a tuple struct??? Could also be an expr_struct.path
                        (
                            PartialRustType::StructIdent(item_generics, item_def.clone()),
                            item_def.js_name.clone(),
                        )
                    }
                    StructEnumUniqueInfo2::Enum(_enum_definition_info) => {
                        // So we are assuming you can't have a path where the final segment is an enum ident
                        panic!()
                    }
                }
            }
            ItemDefRc::Fn(fn_info) => {
                // If turbofish exists on item path segment then use that for type params, otherwise use the unresolved params defined on the fn definition
                let fn_generics = if !item_path.turbofish.is_empty() {
                    item_path
                        .turbofish
                        .iter()
                        .enumerate()
                        .map(|(i, g)| RustTypeParam2 {
                            name: fn_info.generics[i].clone(),
                            type_: RustTypeParamValue2::RustType(Box::new(g.clone())),
                        })
                        .collect::<Vec<_>>()
                } else {
                    fn_info
                        .generics
                        .iter()
                        .map(|g| RustTypeParam2 {
                            name: g.clone(),
                            type_: RustTypeParamValue2::Unresolved,
                        })
                        .collect::<Vec<_>>()
                };

                (
                    PartialRustType::RustType(
                        RustType2::Fn(None, fn_generics, fn_info.clone()),
                        false,
                        false,
                    ),
                    fn_info.js_name.clone(),
                )
            }
            ItemDefRc::Const(const_def) => (
                PartialRustType::RustType(
                    const_def.type_.clone().into_rust_type2(global_data),
                    false,
                    false,
                ),
                const_def.js_name.clone(),
            ),
            _ => todo!(),
        }
    } else {
        // dbg!(segs_copy);
        todo!()
    }
}
