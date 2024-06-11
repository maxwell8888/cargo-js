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
    camel, case_convert, found_item_to_partial_rust_type, get_path, get_path_old, handle_pat,
    handle_syn_stmt::handle_stmt,
    hardcoded_conversions,
    js_ast::{
        DestructureObject, DestructureValue, JsExpr, JsFn, JsIf, JsLocal, JsOp, JsStmt, LocalName,
        LocalType,
    },
    js_stmts_from_syn_items, parse_fn_body_stmts, parse_fn_input_or_field, ConstDef,
    EnumDefinitionInfo, EnumVariantInfo, EnumVariantInputsInfo, FnInfo, GlobalData, ItemDefinition,
    JsImplBlock2, JsImplItem, PartialRustType, RustGeneric, RustImplItem, RustImplItemItem,
    RustImplItemItemNoJs, RustPathSegment, RustTraitDefinition, RustType, RustTypeFnType,
    RustTypeParam, RustTypeParamValue, ScopedVar, StructDefinitionInfo, StructFieldInfo,
    StructOrEnumDefitionInfo,
};

fn handle_expr_assign(
    expr_assign: &ExprAssign,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> JsExpr {
    let (lhs_expr, _lhs_rust_type) = handle_expr(&*expr_assign.left, global_data, current_module);
    let (rhs_expr, _rhs_rust_type) = handle_expr(&*expr_assign.right, global_data, current_module);

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
    fn get_name_and_deref(expr: &Expr) -> (String, bool) {
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
                UnOp::Deref(_) => (get_name_and_deref(&*expr_unary.expr).0, true),
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
    expr: &Expr,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> (JsExpr, RustType) {
    debug!("handle_expr");
    match expr {
        Expr::Array(expr_array) => {
            // TODO how to handle `let a: [i32, 0] = [];`? or other cases where there is no elements and the type is inferred from elsewhere

            let type_ = if expr_array.elems.len() > 0 {
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
                RustType::Array(Box::new(type_)),
            )
        }
        Expr::Assign(expr_assign) => (
            handle_expr_assign(expr_assign, global_data, current_module),
            RustType::Unit,
        ),
        Expr::Async(_) => todo!(),
        Expr::Await(expr_await) => {
            let js_expr = handle_expr(&*expr_await.base, global_data, current_module);
            (JsExpr::Await(Box::new(js_expr.0)), js_expr.1)
        }
        Expr::Binary(expr_binary) => {
            let span = debug_span!("handle_expr_binary", expr_binary = ?quote! { #expr_binary }.to_string());
            let _guard = span.enter();

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
            let (lhs_expr, lhs_type) = handle_expr(&*expr_binary.left, global_data, current_module);

            // TODO need to check for `impl Add for Foo`
            let type_ = match expr_binary.op {
                BinOp::Add(_) | BinOp::Sub(_) | BinOp::Mul(_) | BinOp::Div(_) | BinOp::Rem(_) => {
                    lhs_type.clone()
                }
                BinOp::And(_) | BinOp::Or(_) => RustType::Bool,
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
                | BinOp::Gt(_) => RustType::Bool,
                BinOp::AddAssign(_)
                | BinOp::SubAssign(_)
                | BinOp::MulAssign(_)
                | BinOp::DivAssign(_)
                | BinOp::RemAssign(_) => RustType::Unit,
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
                BinOp::Lt(_) => todo!(),
                BinOp::Le(_) => todo!(),
                BinOp::Ne(_) => todo!(),
                BinOp::Ge(_) => JsOp::GtEq,
                BinOp::Gt(_) => todo!(),
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

            let (rhs_expr, _rhs_type) =
                handle_expr(&*expr_binary.right, global_data, current_module);

            fn types_are_primative(rust_type: RustType) -> bool {
                match rust_type {
                    RustType::I32 => true,
                    RustType::F32 => true,
                    RustType::Bool => true,
                    RustType::String => true,
                    RustType::Option(_) => todo!(),
                    RustType::Result(_) => todo!(),
                    RustType::MutRef(_) => todo!(),
                    RustType::Ref(inner) => types_are_primative(*inner),
                    RustType::Fn(_, _, _, _, _) => todo!(),
                    _ => false,
                }
            }
            let lhs_is_primative = types_are_primative(lhs_type);

            let expr = if lhs_is_primative {
                JsExpr::Binary(Box::new(lhs_expr), js_op, Box::new(rhs_expr))
            } else {
                let lhs = Box::new(JsExpr::Paren(Box::new(lhs_expr)));
                JsExpr::MethodCall(lhs, camel(method_name), vec![rhs_expr])
            };
            (expr, type_)
        }
        Expr::Block(expr_block) => handle_expr_block(expr_block, global_data, current_module),
        Expr::Break(_) => (JsExpr::Break, RustType::NotAllowed),
        Expr::Call(expr_call) => handle_expr_call(expr_call, global_data, current_module),
        Expr::Cast(_) => todo!(),
        Expr::Closure(expr_closure) => {
            handle_expr_closure(expr_closure, global_data, current_module)
        }
        Expr::Const(_) => todo!(),
        Expr::Continue(_) => todo!(),
        Expr::Field(expr_field) => {
            let (base_expr, base_type) =
                handle_expr(&*expr_field.base, global_data, current_module);
            // TODO for a field, the type must be a struct or tuple, so look it up and get the type of the field
            match &expr_field.member {
                Member::Named(ident) => {
                    fn get_field_type(
                        base_type: RustType,
                        global_data: &GlobalData,
                        ident: &Ident,
                    ) -> RustType {
                        match base_type {
                            RustType::StructOrEnum(type_params, module_path, scope_id, name) => {
                                let item_definition = global_data
                                    .lookup_item_def_known_module_assert_not_func2(
                                        &module_path,
                                        &scope_id,
                                        &name,
                                    );
                                match item_definition.struct_or_enum_info {
                                    StructOrEnumDefitionInfo::Struct(struct_def_info) => {
                                        match struct_def_info.fields {
                                            StructFieldInfo::UnitStruct => todo!(),
                                            StructFieldInfo::TupleStruct(_) => todo!(),
                                            StructFieldInfo::RegularStruct(fields) => fields
                                                .iter()
                                                .find_map(|(field_name, field_type)| {
                                                    (field_name == &ident.to_string())
                                                        .then_some(field_type)
                                                })
                                                .unwrap()
                                                .clone(),
                                        }
                                    }
                                    StructOrEnumDefitionInfo::Enum(_) => todo!(),
                                }
                            }
                            RustType::MutRef(inner_type) => {
                                get_field_type(*inner_type, global_data, ident)
                            }
                            _ => {
                                dbg!(&base_type);
                                todo!()
                            }
                        }
                    }
                    let field_type = get_field_type(base_type, global_data, ident);
                    (JsExpr::Field(Box::new(base_expr), camel(ident)), field_type)
                }
                Member::Unnamed(index) => {
                    let type_ = match base_type {
                        RustType::Tuple(tuple_types) => tuple_types[index.index as usize].clone(),
                        RustType::StructOrEnum(type_params, module_path, scope_id, name) => {
                            let item_definition = global_data
                                .lookup_item_def_known_module_assert_not_func2(
                                    &module_path,
                                    &scope_id,
                                    &name,
                                );
                            match item_definition.struct_or_enum_info {
                                StructOrEnumDefitionInfo::Struct(struct_def_info) => {
                                    match struct_def_info.fields {
                                        StructFieldInfo::UnitStruct => todo!(),
                                        StructFieldInfo::TupleStruct(fields) => {
                                            fields[index.index as usize].clone()
                                        }
                                        StructFieldInfo::RegularStruct(_) => todo!(),
                                    }
                                }
                                StructOrEnumDefitionInfo::Enum(_) => todo!(),
                            }
                        }
                        _ => {
                            dbg!(&expr);
                            dbg!(&base_type);
                            todo!()
                        }
                    };
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
        Expr::ForLoop(expr_for_loop) => (
            JsExpr::ForLoop(
                match &*expr_for_loop.pat {
                    Pat::Ident(pat_ident) => camel(&pat_ident.ident),
                    _ => todo!(),
                },
                Box::new(handle_expr(&*expr_for_loop.expr, global_data, current_module).0),
                expr_for_loop
                    .body
                    .stmts
                    .iter()
                    .map(|stmt| handle_stmt(&stmt, global_data, current_module).0)
                    .collect::<Vec<_>>(),
            ),
            RustType::Unit,
        ),
        Expr::Group(_) => todo!(),
        Expr::If(expr_if) => {
            match &*expr_if.cond {
                Expr::Let(_) => todo!(),
                _ => {}
            }

            global_data.push_new_scope(true, Vec::new());
            // TODO block needs to use something like parse_fn_body to be able to return the type

            // NOTE that like match expressions, we can't rely on a single block to get the return type since some might return never/unreachable, so we need to go through each block until we find a non never/unreachable type.
            // TODO For now assume first block return types
            let (mut succeed_stmts, types): (Vec<_>, Vec<_>) = expr_if
                .then_branch
                .stmts
                .iter()
                .map(|stmt| handle_stmt(stmt, global_data, current_module))
                .unzip();

            global_data.pop_scope();
            // update_classes_js_stmts(&mut succeed_stmts, &scope.impl_blocks);

            // TODO handle same as expr::block

            (
                JsExpr::If(JsIf {
                    assignment: None,
                    declare_var: false,
                    condition: Box::new(handle_expr(&*expr_if.cond, global_data, current_module).0),
                    succeed: succeed_stmts,
                    fail: expr_if.else_branch.as_ref().map(|(_, expr)| {
                        Box::new(handle_expr(&*expr, global_data, current_module).0)
                    }),
                }),
                types.last().unwrap().clone(),
            )
        }
        Expr::Index(expr_index) => {
            let (expr, type_) = handle_expr(&*expr_index.expr, global_data, current_module);
            let (index_expr, index_type) =
                handle_expr(&*expr_index.index, global_data, current_module);
            // NOTE `Index` is a trait that can be implemented for any non primitive type (I think?), so need to look up the `Index` impl of the base expr's type to find what the `Output` type is
            // TODO we can use square bracket array[] indexing for arrays, but for other types which don't get transpiled to an array, we need to use `.index(i)` instead
            // "only traits defined in the current crate can be implemented for primitive types"
            // "only traits defined in the current crate can be implemented for arbitrary types"
            // "define and implement a trait or new type instead"
            let (rust_type, use_square_brackets) = match type_ {
                RustType::NotAllowed => todo!(),
                RustType::Todo => todo!(),
                RustType::Unit => todo!(),
                RustType::I32 => todo!(),
                RustType::F32 => todo!(),
                RustType::Bool => todo!(),
                RustType::String => todo!(),
                RustType::StructOrEnum(_, _, _, _) => todo!(),
                // RustType::Enum(_, _) => todo!(),
                RustType::Vec(rust_type) => (*rust_type, true),
                RustType::Array(rust_type) => (*rust_type, true),
                RustType::Tuple(_) => todo!(),
                RustType::MutRef(_) => todo!(),
                RustType::Unknown => todo!(),
                RustType::Never => todo!(),
                RustType::Fn(_, _, _, _, _) => todo!(),
                RustType::Option(_) => todo!(),
                RustType::Result(_) => todo!(),
                RustType::TypeParam(_) => todo!(),
                RustType::ImplTrait(_) => todo!(),
                RustType::ParentItem => todo!(),
                RustType::UserType(_, _) => todo!(),
                RustType::Ref(_) => todo!(),
                RustType::Closure(_) => todo!(),
            };
            (
                JsExpr::Index(Box::new(expr), Box::new(index_expr)),
                rust_type,
            )
        }
        Expr::Infer(_) => todo!(),
        Expr::Let(expr_let) => {
            dbg!(expr_let);
            todo!()
        }
        Expr::Lit(expr_lit) => match &expr_lit.lit {
            Lit::Str(lit_str) => {
                // global_data.rust_prelude_types.string = true;
                // JsExpr::New(
                //     vec!["RustString".to_string()],
                //     vec![JsExpr::LitStr(lit_str.value())],
                // )
                (JsExpr::LitStr(lit_str.value()), RustType::String)
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
                    RustType::I32,
                )
            }
            Lit::Float(lit_float) => {
                global_data.rust_prelude_types.float = true;
                (
                    JsExpr::New(
                        vec!["RustFloat".to_string()],
                        vec![JsExpr::LitFloat(lit_float.base10_parse::<f32>().unwrap())],
                    ),
                    RustType::F32,
                )
            }
            Lit::Bool(lit_bool) => {
                // global_data.rust_prelude_types.bool = true;
                // JsExpr::New(
                //     vec!["RustBool".to_string()],
                //     vec![JsExpr::LitBool(lit_bool.value)],
                // )
                (JsExpr::LitBool(lit_bool.value), RustType::Bool)
            }
            Lit::Verbatim(_) => todo!(),
            _ => todo!(),
        },
        Expr::Loop(expr_loop) => (
            JsExpr::While(
                Box::new(JsExpr::LitBool(true)),
                expr_loop
                    .body
                    .stmts
                    .iter()
                    .map(|stmt| handle_stmt(stmt, global_data, current_module).0)
                    .collect::<Vec<_>>(),
            ),
            RustType::Never,
        ),
        Expr::Macro(expr_macro) => {
            handle_expr_and_stmt_macro(&expr_macro.mac, global_data, current_module)
        }
        Expr::Match(expr_match) => {
            handle_expr_match(expr_match, false, global_data, current_module)
        }
        Expr::MethodCall(expr_method_call) => {
            handle_expr_method_call(expr_method_call, global_data, current_module)
        }
        Expr::Paren(expr_paren) => {
            let (expr, type_) = handle_expr(&*expr_paren.expr, global_data, current_module);
            (JsExpr::Paren(Box::new(expr)), type_)
        }
        Expr::Path(expr_path) => {
            // dbg!("handle_expr expr_path:");
            // dbg!(&expr_path);
            let (js_expr, partial_rust_type) =
                handle_expr_path(expr_path, global_data, current_module, false);
            // dbg!(&partial_rust_type);
            match partial_rust_type {
                // We don't allow `handle_expr()` to be call for tuple struct and enum variant (with args) instantiaion, instead they must must be handled within `handle_expr_call()`
                PartialRustType::StructIdent(_, _, _, _) => panic!(),
                PartialRustType::EnumVariantIdent(_, _, _, _, _) => panic!(),
                PartialRustType::RustType(rust_type) => (js_expr, rust_type),
            }
        }
        Expr::Range(_) => todo!(),
        Expr::Reference(expr_reference) => {
            if expr_reference.mutability.is_some() {
                // Vars need to know whether they are having a mut ref taken to know whether they should copy or not ie `var.inner`, `var.copy()`, or whatever. So we repeat the path handle from `handle_expr()` here so that we can explicitly pass a true argument for `is_having_mut_ref_taken` to handle_expr_path().
                let (expr, rust_type) = match &*expr_reference.expr {
                    Expr::Path(expr_path) => {
                        let (js_expr, partial_rust_type) =
                            handle_expr_path(expr_path, global_data, current_module, true);
                        match partial_rust_type {
                            // We don't allow `handle_expr()` to be call for tuple struct and enum variant (with args) instantiaion, instead they must must be handled within `handle_expr_call()`
                            PartialRustType::StructIdent(_, _, _, _) => panic!(),
                            PartialRustType::EnumVariantIdent(_, _, _, _, _) => panic!(),
                            PartialRustType::RustType(rust_type) => (js_expr, rust_type),
                        }
                    }
                    expr => {
                        let (js_expr, rust_type) = handle_expr(expr, global_data, current_module);

                        // We might already have primative types like RustType::String and RustType::Integer, but now that we are taking a `&mut` we need to ensure that the appropriate prelude flags are set
                        match rust_type {
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
                                (
                                    JsExpr::New(vec!["RustInteger".to_string()], vec![js_expr]),
                                    rust_type,
                                )
                            }
                            RustType::F32 => todo!(),
                            RustType::Bool => todo!(),
                            RustType::String => {
                                global_data.rust_prelude_types.rust_string = true;
                                (
                                    JsExpr::New(vec!["RustString".to_string()], vec![js_expr]),
                                    rust_type,
                                )
                            }
                            RustType::Option(_) => todo!(),
                            RustType::Result(_) => todo!(),
                            RustType::StructOrEnum(_, _, _, _) => (js_expr, rust_type),
                            RustType::Vec(_) => todo!(),
                            RustType::Array(_) => todo!(),
                            RustType::Tuple(_) => todo!(),
                            RustType::UserType(_, _) => todo!(),
                            RustType::MutRef(_) => todo!(),
                            RustType::Ref(_) => todo!(),
                            RustType::Fn(_, _, _, _, _) => todo!(),
                            RustType::Closure(_) => todo!(),
                        }
                    }
                };
                (expr, RustType::MutRef(Box::new(rust_type)))
            } else {
                let (expr, rust_type) =
                    handle_expr(&*expr_reference.expr, global_data, current_module);
                (expr, rust_type)
            }
        }
        Expr::Repeat(_) => todo!(),
        Expr::Return(expr_return) => {
            if let Some(expr) = &expr_return.expr {
                // If return is the deref of a &mut, then `.copy()` it
                let (js_expr, type_) = match &**expr {
                    Expr::Unary(expr_unary) => match expr_unary.op {
                        UnOp::Deref(_) => {
                            let (expr, type_) = handle_expr(expr, global_data, current_module);
                            (
                                JsExpr::MethodCall(Box::new(expr), "copy".to_string(), Vec::new()),
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
        Expr::Struct(expr_struct) => {
            // NOTE remember this handles both struct and enum variant instantiation eg `MyStruct {}` and `MyEnum::Variant { num: 0 }`
            // dbg!("handle expr_struct");
            // println!("{}", quote! { #expr_struct });
            let (js_path_expr, rust_partial_type) =
                handle_expr_path_inner(&expr_struct.path, global_data, current_module, false);

            // dbg!(&js_path_expr);
            // dbg!(&rust_partial_type);
            match rust_partial_type {
                PartialRustType::StructIdent(type_params, module_path, scope_id, name) => {
                    let js_deduped_path = match js_path_expr.clone() {
                        JsExpr::Path(path) => {
                            assert_eq!(path.len(), 1);
                            path
                        }
                        _ => todo!(),
                    };
                    let rust_type =
                        RustType::StructOrEnum(type_params, module_path, scope_id, name.clone());
                    let args = expr_struct
                        .fields
                        .iter()
                        .map(|field| handle_expr(&field.expr, global_data, current_module).0)
                        .collect::<Vec<_>>();
                    // TODO IMPORTANT need to be using deduplicated name here
                    (JsExpr::New(js_deduped_path, args), rust_type)
                }
                PartialRustType::EnumVariantIdent(
                    type_params,
                    module_path,
                    scope_id,
                    enum_name,
                    variant_name,
                ) => {
                    let rust_type = RustType::StructOrEnum(
                        type_params,
                        module_path,
                        scope_id,
                        enum_name.clone(),
                    );
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
                PartialRustType::RustType(_) => todo!(),
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
        Expr::Try(_) => todo!(),
        Expr::TryBlock(_) => todo!(),
        Expr::Tuple(_) => todo!(),
        Expr::Unary(expr_unary) => match expr_unary.op {
            UnOp::Deref(_) => {
                let (expr, rust_type) = handle_expr(&*expr_unary.expr, global_data, current_module);
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
                    RustType::MutRef(inner_type) => {
                        let add_inner = match *inner_type {
                            RustType::NotAllowed => todo!(),
                            RustType::Unknown => todo!(),
                            RustType::Todo => todo!(),
                            RustType::ParentItem => todo!(),
                            RustType::Unit => todo!(),
                            RustType::Never => todo!(),
                            RustType::ImplTrait(_) => todo!(),
                            RustType::TypeParam(_) => todo!(),
                            RustType::I32 => true,
                            RustType::F32 => true,
                            RustType::Bool => true,
                            RustType::String => true,
                            RustType::Option(_) => todo!(),
                            RustType::Result(_) => todo!(),
                            RustType::StructOrEnum(type_params, module_path, scope_id, name) => {
                                dbg!(type_params);
                                dbg!(module_path);
                                dbg!(scope_id);
                                dbg!(name);
                                todo!();
                            }
                            RustType::Vec(_) => todo!(),
                            RustType::Array(_) => todo!(),
                            RustType::Tuple(_) => todo!(),
                            RustType::UserType(_, _) => todo!(),
                            RustType::MutRef(_) => todo!(),
                            RustType::Ref(_) => todo!(),
                            RustType::Fn(_, _, _, _, _) => todo!(),
                            RustType::Closure(_) => todo!(),
                        };

                        let new_expr = if add_inner {
                            JsExpr::Field(Box::new(expr), "inner".to_string())
                        } else {
                            expr
                        };
                        (new_expr, *inner_type)
                    }
                    // TODO I though we didn't care about & types, only &mut ???
                    RustType::Ref(_) => todo!(),
                    rust_type => (expr, rust_type),
                }
            }
            UnOp::Not(_) => (
                JsExpr::Not(Box::new(
                    handle_expr(&*expr_unary.expr, global_data, current_module).0,
                )),
                RustType::Bool,
            ),
            UnOp::Neg(_) => {
                let (expr, type_) = handle_expr(&*expr_unary.expr, global_data, current_module);
                (JsExpr::Minus(Box::new(expr)), type_)
            }
            _ => todo!(),
        },
        Expr::Unsafe(_) => todo!(),
        Expr::Verbatim(_) => todo!(),
        Expr::While(expr_while) => (
            JsExpr::While(
                Box::new(handle_expr(&*expr_while.cond, global_data, current_module).0),
                expr_while
                    .body
                    .stmts
                    .iter()
                    .map(|stmt| handle_stmt(stmt, global_data, current_module).0)
                    .collect::<Vec<_>>(),
            ),
            RustType::Unit,
        ),
        Expr::Yield(_) => todo!(),
        _ => todo!(),
    }
}

fn handle_expr_closure(
    expr_closure: &ExprClosure,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> (JsExpr, RustType) {
    let async_ = match &*expr_closure.body {
        Expr::Async(_) => true,
        _ => false,
    };

    let body_is_js_block = match &*expr_closure.body {
        Expr::Async(expr_async) => {
            // If we have a single statement which is an expression that has no semi so is being returned then in Rust async we have to put it in a block but in Javascript we don't need to

            // multi lines should be in blocks
            let stmts = &expr_async.block.stmts;
            if stmts.len() > 1 {
                true
            } else {
                let is_expr_with_no_semi = match stmts.last().unwrap() {
                    Stmt::Expr(_, semi) => semi.is_none(),
                    Stmt::Macro(_) => todo!(),
                    _ => false,
                };
                if is_expr_with_no_semi {
                    false
                } else {
                    true
                }
            }
        }
        Expr::Block(_) => true,
        Expr::Match(_) => true,
        _ => false,
    };

    let inputs = expr_closure
        .inputs
        .iter()
        .map(|input| match input {
            Pat::Ident(pat_ident) => camel(&pat_ident.ident),
            Pat::Tuple(_) => todo!(),
            Pat::Type(pat_type) => {
                let name = match &*pat_type.pat {
                    Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
                    _ => todo!(),
                };
                camel(name)
            }
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
    global_data.push_new_scope(false, Vec::new());

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
    for input in &expr_closure.inputs {
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
        } else {
            todo!();
            // Could in theory return Uknown and resolve the type later, but even Rust will often error with:
            // `type must be known at this point. consider giving this closure parameter an explicit type.`
            // So need think more about which cases need type annotations and which don't, so just assume type annotations for now.
            RustType::Unknown
        };

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
                lhs: LocalName::Single(input_name.clone()),
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
                        JsExpr::New(
                            vec!["RustInteger".to_string()],
                            vec![JsExpr::MethodCall(
                                Box::new(JsExpr::Path(vec![input_name])),
                                "inner".to_string(),
                                Vec::new(),
                            )],
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
                    RustType::Closure(_) => todo!(),
                },
            }))
        }
    }

    // Need to handle different Expr's separately because Expr::Match needs passing an arg that it is being returned. Not sure the other cases are necessary
    let (body_stmts, return_type) = match &*expr_closure.body {
        Expr::Block(expr_block) => parse_fn_body_stmts(
            true,
            false,
            &expr_block.block.stmts,
            global_data,
            current_module,
        ),
        Expr::Async(expr_async) => parse_fn_body_stmts(
            true,
            false,
            &expr_async.block.stmts,
            global_data,
            current_module,
        ),
        Expr::Match(expr_match) => {
            let (expr, type_) = handle_expr_match(expr_match, true, global_data, current_module);
            (vec![JsStmt::Expr(expr, false)], type_)
        }
        other => {
            let (expr, type_) = handle_expr(other, global_data, current_module);
            (vec![JsStmt::Expr(expr, false)], type_)
        }
    };

    global_data.pop_scope();

    (
        JsExpr::ArrowFn(async_, body_is_js_block, inputs, body_stmts),
        // NOTE closures cannot be generic
        // RustType::Fn(None, Vec::new(), None, RustTypeFnType::Standalone(())),
        RustType::Closure(Box::new(return_type)),
    )
}

// TODO might want to split this up so that in some cases we can return JsStmt and JsExpr in others
pub fn handle_expr_and_stmt_macro(
    mac: &Macro,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> (JsExpr, RustType) {
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
                handle_expr(elem, global_data, current_module).1
            } else {
                RustType::Unknown
            };
            let expr_vec = expr_array
                .elems
                .iter()
                .map(|elem| handle_expr(elem, global_data, current_module).0)
                .collect::<Vec<_>>();
            return (JsExpr::Array(expr_vec), RustType::Vec(Box::new(vec_type)));
        }
        if path_segs[0] == "panic" {
            let input = mac.tokens.clone().to_string();
            // TODO use a custom error so it isn't inadvertently caught by other JS code in the app?
            return (JsExpr::ThrowError(input), RustType::Never);
        }
        if path_segs[0] == "try_" {
            let input = mac.tokens.clone().to_string();
            let try_block = syn::parse_str::<syn::Block>(&input).unwrap();
            let stmt_vec = try_block
                .stmts
                .iter()
                .map(|stmt| handle_stmt(stmt, global_data, current_module).0)
                .collect::<Vec<_>>();
            return (JsExpr::TryBlock(stmt_vec), RustType::Unit);
        }
        if path_segs[0] == "catch" {
            let input = mac.tokens.clone().to_string();
            let mut parts = input.split(",");
            let err_var_name = parts.next().unwrap();
            let err_var_name = syn::parse_str::<syn::Ident>(err_var_name)
                .unwrap()
                .to_string();
            let _err_var_type = parts.next().unwrap();
            let catch_block = parts.collect::<String>();
            let catch_block = syn::parse_str::<syn::Block>(&catch_block).unwrap();
            let stmt_vec = catch_block
                .stmts
                .into_iter()
                .map(|stmt| handle_stmt(&stmt, global_data, current_module).0);
            let stmt_vec = stmt_vec.collect::<Vec<_>>();
            return (JsExpr::CatchBlock(err_var_name, stmt_vec), RustType::Unit);
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
                        handle_expr(&condition_expr, global_data, current_module).0,
                    ))),
                    "jsBoolean".to_string(),
                )
            } else {
                handle_expr(&condition_expr, global_data, current_module).0
            };
            warn!("after");

            return (
                JsExpr::MethodCall(
                    Box::new(JsExpr::Path(vec!["console".to_string()])),
                    "assert".to_string(),
                    vec![condition_js],
                ),
                RustType::Unit,
            );
        }
        if path_segs[0] == "assert_eq" {
            let input = mac.tokens.clone().to_string();
            let mut parts = input.split(",");

            let lhs = parts.next().unwrap();
            let syn_lhs = syn::parse_str::<syn::Expr>(lhs).unwrap();
            let lhs = handle_expr(&syn_lhs, global_data, current_module);

            let rhs = parts.next().unwrap();
            let rhs = syn::parse_str::<syn::Expr>(rhs).unwrap();
            let rhs = handle_expr(&rhs, global_data, current_module);

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
                            .map_or((false, false, false), |ScopedVar { name, mut_, type_ }| {
                                let is_primative = match type_ {
                                    RustType::Todo => {
                                        // global_data
                                        //     .rust_prelude_types
                                        //     .number_prototype_extensions = true;
                                        // global_data
                                        //     .rust_prelude_types
                                        //     .string_prototype_extensions = true;
                                        false
                                    }
                                    RustType::Unit => true,
                                    RustType::I32 => true,
                                    RustType::F32 => true,
                                    RustType::Bool => true,
                                    RustType::String => true,
                                    RustType::StructOrEnum(_, _, _, _) => false,
                                    // RustType::Enum(_) => false,
                                    RustType::NotAllowed => false,
                                    RustType::Unknown => false,
                                    RustType::Never => false,
                                    RustType::Vec(_) => false,
                                    RustType::Array(_) => false,
                                    RustType::Tuple(_) => false,
                                    RustType::MutRef(_) => false,
                                    RustType::Fn(_, _, _, _, _) => false,
                                    RustType::Option(_) => false,
                                    RustType::Result(_) => false,
                                    RustType::TypeParam(_) => false,
                                    RustType::ImplTrait(_) => false,
                                    RustType::ParentItem => todo!(),
                                    RustType::UserType(_, _) => todo!(),
                                    RustType::Ref(inner) => match &**inner {
                                        RustType::String => true,
                                        _ => todo!(),
                                    },
                                    RustType::Closure(_) => todo!(),
                                };
                                let mut_ref = match type_ {
                                    RustType::MutRef(_) => true,
                                    _ => false,
                                };
                                (*mut_, mut_ref, is_primative)
                            })
                    } else {
                        (false, false, false)
                    }
                }
                _ => (false, false, false),
            };

            let mut equality_check = if !lhs_is_primative || lhs_is_mut || lhs_is_mut_ref {
                // global_data.rust_prelude_types.number_prototype_extensions = true;
                // global_data.rust_prelude_types.string_prototype_extensions = true;
                JsExpr::MethodCall(Box::new(lhs.0), "eq".to_string(), vec![rhs.0])
            } else {
                JsExpr::Binary(Box::new(lhs.0), JsOp::Eq, Box::new(rhs.0))
            };

            // let equality_check = JsExpr::Binary(Box::new(lhs), JsOp::Eq, Box::new(rhs));
            let bool_is_mut = false;
            if bool_is_mut {
                equality_check = JsExpr::Field(
                    Box::new(JsExpr::Paren(Box::new(equality_check))),
                    "jsBoolean".to_string(),
                );
            }
            return (
                JsExpr::MethodCall(
                    Box::new(JsExpr::Path(vec!["console".to_string()])),
                    "assert".to_string(),
                    vec![equality_check],
                ),
                RustType::Unit,
            );
        }
    }
    todo!()
}

fn handle_expr_method_call(
    expr_method_call: &ExprMethodCall,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> (JsExpr, RustType) {
    let method_name = expr_method_call.method.to_string();
    let span = debug_span!("handle_expr_method_call", expr_method_call = ?quote! { #expr_method_call }.to_string());
    let _guard = span.enter();

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

    let mut method_name = expr_method_call.method.to_string();
    let (receiver, receiver_type) =
        handle_expr(&*expr_method_call.receiver, global_data, current_module);

    let (args_js_exprs, args_rust_types): (Vec<_>, Vec<_>) = expr_method_call
        .args
        .iter()
        .map(|arg| handle_expr(arg, global_data, current_module))
        .unzip();

    // get method type
    fn get_method_return_type(
        receiver_type: RustType,
        method_name: &String,
        global_data: &GlobalData,
        receiver: &JsExpr,
        expr_method_call: &ExprMethodCall,
        current_module: &Vec<String>,
        args_rust_types: Vec<RustType>,
    ) -> RustType {
        match receiver_type {
            RustType::NotAllowed => todo!(),
            RustType::Unknown => todo!(),
            RustType::Todo => todo!(),
            RustType::ParentItem => todo!(),
            RustType::Unit => todo!(),
            RustType::Never => todo!(),
            RustType::ImplTrait(_) => todo!(),
            RustType::TypeParam(_) => todo!(),
            RustType::I32 => todo!(),
            RustType::F32 => todo!(),
            RustType::Bool => todo!(),
            RustType::String => {
                // if method_name == "to_string" || method_name == "clone" {
                //     return (receiver.clone(), RustType::String);
                // } else {
                //     todo!()
                // }
                if method_name == "to_string" || method_name == "clone" || method_name == "push_str"
                {
                    RustType::String
                } else {
                    todo!()
                }
            }
            RustType::Option(_) => todo!(),
            RustType::Result(_) => todo!(),
            RustType::StructOrEnum(
                item_type_params,
                item_module_path,
                item_scope_id,
                item_name,
            ) => {
                let item_def = global_data.lookup_item_def_known_module_assert_not_func2(
                    &item_module_path,
                    &item_scope_id,
                    &item_name,
                );

                let method_turbofish_rust_types =
                    expr_method_call.turbofish.as_ref().map(|generics| {
                        generics
                            .args
                            .iter()
                            .map(|generic_arg| match generic_arg {
                                GenericArgument::Lifetime(_) => todo!(),
                                GenericArgument::Type(type_) => {
                                    let (type_params, module_path, scope_id, name) = global_data
                                        .syn_type_to_rust_type_struct_or_enum(
                                            current_module,
                                            type_,
                                        );
                                    RustType::StructOrEnum(type_params, module_path, scope_id, name)
                                }
                                GenericArgument::Const(_) => todo!(),
                                GenericArgument::AssocType(_) => todo!(),
                                GenericArgument::AssocConst(_) => todo!(),
                                GenericArgument::Constraint(_) => todo!(),
                                _ => todo!(),
                            })
                            .collect::<Vec<_>>()
                    });

                let sub_path = RustPathSegment {
                    ident: method_name.clone(),
                    turbofish: method_turbofish_rust_types.clone().unwrap_or(Vec::new()),
                };

                // let impl_method = if let Some(impl_method) = global_data.lookup_impl_item_item(
                //     &item_type_params,
                //     &item_module_path,
                //     &item_scope_id,
                //     &sub_path,
                //     &item_name,
                //     &item_def,
                // ) {
                //     impl_method
                // } else {
                //     // dbg!(&global_data.scopes);
                //     dbg!(&item_type_params);
                //     dbg!(&item_module_path);
                //     dbg!(&item_scope_id);
                //     dbg!(&sub_path);
                //     dbg!(&item_name);
                //     dbg!(&item_def);
                //     panic!()
                // };

                let impl_method = global_data
                    .lookup_impl_item_item2(
                        &item_type_params,
                        &item_module_path,
                        &item_scope_id,
                        &item_def,
                        &sub_path,
                    )
                    .unwrap();

                fn method_return_type_generic_resolve_to_rust_type(
                    item_type_params: &Vec<RustTypeParam>,
                    rust_type_param: &RustTypeParam,
                    fn_info: &FnInfo,
                    method_turbofish_rust_types: &Option<Vec<RustType>>,
                    args_rust_types: &Vec<RustType>,
                ) -> RustType {
                    // Is type param defined on item?
                    if let Some(item_type_param) = item_type_params
                        .iter()
                        .find(|item_type_param| item_type_param.name == rust_type_param.name)
                    {
                        match &item_type_param.type_ {
                            RustTypeParamValue::Unresolved => todo!(),
                            RustTypeParamValue::RustType(rust_type) => *rust_type.clone(),
                        }
                        // Is type param specified in  on method (and we have a method turbofish)?
                    } else if let Some(gen_index) =
                        fn_info.generics.iter().position(|method_type_param| {
                            method_type_param == &rust_type_param.name
                                && method_turbofish_rust_types.is_some()
                        })
                    {
                        method_turbofish_rust_types.as_ref().unwrap()[gen_index].clone()

                        // Is type param concrete value resolved by input argument?
                    } else if let Some(pos) =
                        fn_info
                            .inputs_types
                            .iter()
                            .position(|input_type| match input_type {
                                RustType::TypeParam(input_rust_type_param) => {
                                    input_rust_type_param.name == rust_type_param.name
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

                match impl_method.item {
                    RustImplItemItemNoJs::Fn(private, static_, fn_info) => {
                        // If return type has generics, replace them with the concretised generics that exist on the receiver item, the method's turbofish, or the methods arguments
                        let return_type = fn_info.return_type.clone();
                        fn resolve_generics_for_return_type(
                            return_type: RustType,
                            item_type_params: &Vec<RustTypeParam>,
                            fn_info: &FnInfo,
                            method_turbofish_rust_types: &Option<Vec<RustType>>,
                            args_rust_types: &Vec<RustType>,
                        ) -> RustType {
                            match return_type {
                                RustType::NotAllowed => todo!(),
                                RustType::Unknown => todo!(),
                                RustType::Todo => todo!(),
                                RustType::ParentItem => todo!(),
                                RustType::Unit => RustType::Unit,
                                RustType::Never => todo!(),
                                RustType::ImplTrait(_) => todo!(),
                                RustType::TypeParam(rust_type_param) => {
                                    // type param is unresolved at this point
                                    assert!(match rust_type_param.type_ {
                                        RustTypeParamValue::Unresolved => true,
                                        RustTypeParamValue::RustType(_) => false,
                                    });

                                    method_return_type_generic_resolve_to_rust_type(
                                        &item_type_params,
                                        &rust_type_param,
                                        &fn_info,
                                        &method_turbofish_rust_types,
                                        &args_rust_types,
                                    )
                                }
                                RustType::I32 => RustType::I32,
                                RustType::F32 => RustType::F32,
                                RustType::Bool => RustType::Bool,
                                RustType::String => RustType::String,
                                RustType::Option(_) => todo!(),
                                RustType::Result(_) => todo!(),
                                RustType::StructOrEnum(
                                    type_params,
                                    module_path,
                                    scope_id,
                                    name,
                                ) => {
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
                                                    let rust_type =
                                                        method_return_type_generic_resolve_to_rust_type(
                                                            &item_type_params,
                                                            tp,
                                                            &fn_info,
                                                            &method_turbofish_rust_types,
                                                            &args_rust_types,
                                                        );
                                                    new_tp.type_ = RustTypeParamValue::RustType(
                                                        Box::new(rust_type),
                                                    );
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
                                        name.clone(),
                                    )
                                }
                                RustType::Vec(_) => todo!(),
                                RustType::Array(_) => todo!(),
                                RustType::Tuple(_) => todo!(),
                                RustType::UserType(_, _) => todo!(),
                                RustType::MutRef(_) => todo!(),
                                RustType::Ref(inner_type) => {
                                    RustType::Ref(Box::new(resolve_generics_for_return_type(
                                        *inner_type,
                                        item_type_params,
                                        fn_info,
                                        method_turbofish_rust_types,
                                        args_rust_types,
                                    )))
                                }
                                RustType::Fn(_, _, _, _, _) => todo!(),
                                RustType::Closure(_) => todo!(),
                            }
                        }
                        resolve_generics_for_return_type(
                            return_type,
                            &item_type_params,
                            &fn_info,
                            &method_turbofish_rust_types,
                            &args_rust_types,
                        )
                    }
                    RustImplItemItemNoJs::Const => todo!(),
                }
            }
            RustType::Vec(element) => {
                // TODO we are assuming `.collect::<Vec<_>>()` here but should support other `.collect()`s
                if method_name == "iter" || method_name == "collect" {
                    RustType::Vec(element)
                } else if method_name == "map" {
                    let closure_return = match &args_rust_types[0] {
                        RustType::MutRef(_) => todo!(),
                        RustType::Ref(_) => todo!(),
                        RustType::Fn(_, _, _, _, _) => todo!(),
                        RustType::Closure(return_type) => return_type.clone(),
                        _ => todo!(),
                    };
                    RustType::Vec(closure_return)
                } else {
                    todo!()
                }
            }
            RustType::Array(element) => {
                // TODO need to think about the different between Array and Vec here
                // TODO we are assuming `.collect::<Vec<_>>()` here but should support other `.collect()`s
                if method_name == "iter" || method_name == "collect" {
                    RustType::Array(element)
                } else if method_name == "map" {
                    let closure_return = match &args_rust_types[0] {
                        RustType::MutRef(_) => todo!(),
                        RustType::Ref(_) => todo!(),
                        RustType::Fn(_, _, _, _, _) => todo!(),
                        RustType::Closure(return_type) => return_type.clone(),
                        _ => todo!(),
                    };
                    RustType::Array(closure_return)
                } else {
                    todo!()
                }
            }
            RustType::Tuple(_) => todo!(),
            RustType::UserType(_, _) => todo!(),
            RustType::MutRef(inner_rust_type) => get_method_return_type(
                *inner_rust_type,
                method_name,
                global_data,
                receiver,
                expr_method_call,
                current_module,
                args_rust_types,
            ),
            RustType::Ref(_) => todo!(),
            RustType::Fn(_, _, _, _, _) => todo!(),
            RustType::Closure(_) => todo!(),
        }
    }
    let method_return_type = get_method_return_type(
        receiver_type.clone(),
        &method_name,
        global_data,
        &receiver,
        expr_method_call,
        current_module,
        args_rust_types,
    );

    let receiver_is_mut_var = match &*expr_method_call.receiver {
        Expr::Path(expr_path) if expr_path.path.segments.len() == 1 => {
            // IMPORTANT TODO limit to transparent scopes
            global_data.scopes.iter().rev().any(|s| {
                s.variables
                    .iter()
                    .any(|v| v.name == expr_path.path.segments[0].ident.to_string() && v.mut_)
            })
        }
        _ => false,
    };
    // TODO IMPORTANT can't assume that `.to_string()` etc called on are string are the std lib impls, since they might have been overwritten/shadowed by local trait impls, so need to first check for local impls.
    if let RustType::String = &receiver_type {
        if method_name == "to_string" || method_name == "clone" {
            // NOTE ASSUMPTION if the receiver a type RustType::String and also has a RustString wrapper (and this needs `.inner` calling) then it the receiver *must* be a `mut` var. Whilst receivers other than a `mut` var can have a RustString wrapper and call `.to_string()`, `.clone()`, etc, all those receivers *must* be a RustType::MutRef(RustType::String), which is handled below.
            // If receiver var has a RustString wrapper ie is `mut`, then take the inner
            if receiver_is_mut_var {
                return (
                    JsExpr::Field(Box::new(receiver.clone()), "inner".to_string()),
                    RustType::String,
                );
            } else {
                return (receiver.clone(), RustType::String);
            }
        } else if method_name == "push_str" {
            assert_eq!(args_js_exprs.len(), 1);
            if receiver_is_mut_var {
                return (
                    JsExpr::Binary(
                        Box::new(receiver.clone()),
                        JsOp::AddAssign,
                        Box::new(args_js_exprs[0].clone()),
                    ),
                    RustType::String,
                );
            } else {
                panic!("can't call push_str on a var which is not mutable")
            }
        } else {
            todo!()
        }
    }
    if let RustType::MutRef(inner) = &receiver_type {
        if let RustType::String = **inner {
            if method_name == "to_string" || method_name == "clone" {
                // Receiver is &mut so has a RustString wrapper so take the inner
                return (
                    JsExpr::Field(Box::new(receiver.clone()), "inner".to_string()),
                    RustType::String,
                );
            } else if method_name == "push_str" {
                assert_eq!(args_js_exprs.len(), 1);
                return (
                    JsExpr::Binary(
                        Box::new(receiver.clone()),
                        JsOp::AddAssign,
                        Box::new(args_js_exprs[0].clone()),
                    ),
                    RustType::String,
                );
            } else {
                todo!()
            }
        }
    }

    if let RustType::Array(_) = receiver_type {
        if method_name == "iter" || method_name == "collect" {
            return (receiver, method_return_type);
        }
    }
    if let RustType::Vec(_) = receiver_type {
        if method_name == "iter" || method_name == "collect" {
            return (receiver, method_return_type);
        }
    }

    if let JsExpr::Path(path) = &receiver {
        if path.len() == 2 {
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
                            name: "jsonParse".to_string(),
                            input_names: vec!["text".to_string()],
                            body_stmts: vec![JsStmt::Raw(body)],
                        })))),
                        expr_method_call
                            .args
                            .iter()
                            .map(|arg| handle_expr(arg, global_data, current_module).0)
                            .collect::<Vec<_>>(),
                    ),
                    RustType::Todo,
                );
            }
        }
    }
    // if method_name == "iter" {
    //     return (receiver, RustType::Todo);
    // }
    // if method_name == "collect" {
    //     return (receiver, RustType::Todo);
    // }
    if method_name.len() > 3 && &method_name[0..3] == "js_" {
        method_name = method_name[3..].to_string();
    }
    // if method_name == "is_some" {
    //     return JsExpr::Binary(Box::new(receiver), JsOp::NotEq, Box::new(JsExpr::Null));
    // }
    // if method_name == "slice1" || method_name == "slice2" {
    //     method_name = "slice".to_string();
    // }
    if let Some(last_char) = method_name.chars().last() {
        if last_char.is_digit(10) {
            method_name.pop().unwrap();
            // method_name = method_name[..method_name.len() - 1].to_string();
        }
    }
    if method_name == "add_event_listener_async" {
        method_name = "add_event_listener".to_string();
    }
    if method_name == "length" {
        return (
            JsExpr::Field(Box::new(receiver), "length".to_string()),
            RustType::I32,
        );
    }
    (
        JsExpr::MethodCall(Box::new(receiver), camel(method_name), args_js_exprs),
        method_return_type,
    )
}

pub fn handle_expr_block(
    expr_block: &ExprBlock,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> (JsExpr, RustType) {
    // let span = debug_span!("handle_expr_block", expr_block = ?quote! { #expr_block }.to_string());
    let span = debug_span!("handle_expr_block");
    let _guard = span.enter();

    global_data.push_new_scope(true, Vec::new());
    // TODO block needs to use something like parse_fn_body to be able to return the type
    let (mut stmts, types): (Vec<_>, Vec<_>) = expr_block
        .block
        .stmts
        .iter()
        .map(|stmt| handle_stmt(stmt, global_data, current_module))
        .unzip();

    // pop block scope
    global_data.pop_scope();
    // update_classes_js_stmts(&mut stmts, &scope.unwrap().impl_blocks);

    (JsExpr::Block(stmts), types.last().unwrap().clone())
}

fn handle_expr_call(
    expr_call: &ExprCall,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> (JsExpr, RustType) {
    // dbg!(expr_call);
    let js_primitive = match &*expr_call.func {
        Expr::Path(expr_path) => {
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

    let args = expr_call
        .args
        .iter()
        .map(|arg| handle_expr(arg, global_data, current_module))
        .collect::<Vec<_>>();
    let args_js_expr = args.iter().map(|a| a.0.clone()).collect::<Vec<_>>();
    let args_rust_types = args.iter().map(|a| a.1.clone()).collect::<Vec<_>>();

    // // handle tuple structs Some, Ok, Err
    // match &*expr_call.func {
    //     Expr::Path(expr_path) => {
    //         let path = expr_path
    //             .path
    //             .segments
    //             .iter()
    //             .map(|seg| seg.ident.to_string())
    //             .collect::<Vec<_>>();
    //         let name = path.last().unwrap();
    //         // TODO need to properly identify what is an enum variant and what is a tuple struct. For now assume paths with length 1 are tuple structs
    //         if path.len() == 1
    //             && name.chars().next().unwrap().is_ascii_uppercase()
    //             && name != "Some"
    //             && name != "Ok"
    //             && name != "Err"
    //         {
    //             return (JsExpr::New(path, args_js_expr.clone()), RustType::Todo);
    //         }
    //     }
    //     _ => {}
    // }

    // record if using Some/Option
    match &*expr_call.func {
        Expr::Path(expr_path) => {
            let last = expr_path.path.segments.last().unwrap().ident.to_string();
            if last == "Some" {
                global_data.rust_prelude_types.option = true;
                global_data.rust_prelude_types.some = true;
            }
        }
        _ => {}
    }

    // Determine return type, looking up structs/enums in scope and checking for generics and whether they can be inferred
    // TODO also need to handle looking up fns
    // TODO clean up handle_expr_local where this was before

    // parse fn call
    match &*expr_call.func {
        Expr::Path(expr_path) => {
            if let Some(js_expr) = hardcoded_conversions(expr_path, args_js_expr.clone()) {
                return js_expr;
            }

            let (expr, partial_rust_type) =
                handle_expr_path(expr_path, global_data, current_module, false);

            let rust_type = match partial_rust_type.clone() {
                // If a struct or enum variant is called, it must be a tuple strut of enum variant
                PartialRustType::StructIdent(type_params, module_path, scope_id, struct_name) => {
                    let item_def = global_data.lookup_item_def_known_module_assert_not_func2(
                        &module_path,
                        &scope_id,
                        &struct_name,
                    );

                    let struct_def = match item_def.struct_or_enum_info {
                        StructOrEnumDefitionInfo::Struct(struct_def) => struct_def,
                        StructOrEnumDefitionInfo::Enum(_) => panic!(),
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
                                        .map(|(name, type_)| type_)
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
                                    new_tp.type_ = RustTypeParamValue::RustType(Box::new(
                                        args_rust_types[i].clone(),
                                    ));
                                    return new_tp;
                                }
                            }
                            tp.clone()
                        })
                        .collect::<Vec<_>>();

                    RustType::StructOrEnum(updated_type_params, module_path, scope_id, struct_name)
                }
                PartialRustType::EnumVariantIdent(
                    type_params,
                    module_path,
                    scope_id,
                    enum_name,
                    variant_name,
                ) => {
                    let item_def = global_data.lookup_item_def_known_module_assert_not_func2(
                        &module_path,
                        &scope_id,
                        &enum_name,
                    );

                    let enum_def = match item_def.struct_or_enum_info {
                        StructOrEnumDefitionInfo::Struct(_) => panic!(),
                        StructOrEnumDefitionInfo::Enum(enum_def) => enum_def,
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
                                    return RustTypeParam {
                                        name: tp.name.clone(),
                                        type_: RustTypeParamValue::RustType(Box::new(
                                            args_rust_types[i].clone(),
                                        )),
                                    };
                                }
                            }
                            tp.clone()
                        })
                        .collect::<Vec<_>>();

                    RustType::StructOrEnum(updated_type_params, module_path, scope_id, enum_name)
                }
                PartialRustType::RustType(rust_type) => {
                    // handle_expr_path checks if the path is any scoped/module level fn, enum variant, tuple struct, associated fn, or var with one of these types, but it doesn't know the args the path is being called with so it is at this point that we check if any generics can be made concrete
                    // We also resolved the type to whatever the call returns eg fn path -> fn return type, enum variant path -> enum instance, tuple struct path -> struct instance, etc
                    match rust_type {
                        RustType::ImplTrait(_) => todo!(),
                        RustType::TypeParam(_) => {
                            // TODO if type param is resolved it could be a fn or whatever so need to call this match recursively/as a fn
                            // otherwise need return the unresolved type but also note that it has been called(!) so could be the return type of some fn yet to be known, a tuple struct instance, and enum instance, etc. Though this seems like a rarer case and I'm not sure it is even possbile/aloud to wait until after calling a type param to resolved
                            todo!()
                        }
                        RustType::Fn(
                            item_type_params,
                            type_params,
                            module_path,
                            scope_id,
                            rust_type_fn_type,
                        ) => {
                            // let name = match name {
                            let fn_info = match rust_type_fn_type {
                                RustTypeFnType::Standalone(name) => global_data
                                    .lookup_fn_info_known_module(&module_path, &scope_id, &name),
                                RustTypeFnType::AssociatedFn(item_name, fn_name) => {
                                    let item_type_params = item_type_params.unwrap();
                                    let item_definition = global_data
                                        .lookup_item_def_known_module_assert_not_func2(
                                            &module_path,
                                            &scope_id,
                                            &item_name.clone(),
                                        );
                                    let impl_method = global_data
                                        .lookup_impl_item_item2(
                                            &item_type_params,
                                            &module_path,
                                            &scope_id,
                                            &item_definition,
                                            // TODO IMPORTANT not populating turbofish correctly
                                            &RustPathSegment {
                                                ident: fn_name.clone(),
                                                turbofish: Vec::new(),
                                            },
                                        )
                                        .unwrap();
                                    match impl_method.item {
                                        RustImplItemItemNoJs::Fn(_, _, fn_info) => fn_info,
                                        RustImplItemItemNoJs::Const => todo!(),
                                    }
                                }
                            };

                            // Look to see if any of the input types are type params replace with concrete type from argument
                            // TODO IMPORTANT for an associated fn, attempt_to_resolve_type_params_using_arg_types will not take into account generics defined on the *item* which might be concretised by the args and used in the return type
                            let new_type_params = fn_info
                                .attempt_to_resolve_type_params_using_arg_types(&args_rust_types);

                            // Resolve return type
                            // RustType::Fn returns a different type when called (which is obviously what is happening given we are handling expr_call), and also might be nested, ie inside other types that take generics eg Some(fn_returns_i32()) => RustType::Option(RustType::i32), so need to resolve this
                            fn get_fn_type_returns(
                                return_type: RustType,
                                current_type_params: &Vec<RustTypeParam>,
                            ) -> RustType {
                                match &return_type {
                                    RustType::Unknown => todo!(),
                                    RustType::Todo => todo!(),
                                    RustType::ParentItem => todo!(),
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
                                            RustTypeParamValue::Unresolved => return_type,
                                            RustTypeParamValue::RustType(resolved_type) => {
                                                *resolved_type
                                            }
                                        }
                                    }
                                    RustType::Option(rust_type) => todo!(),
                                    RustType::Result(_) => todo!(),
                                    // RustType::StructOrEnum(_, _, _) => todo!(),
                                    RustType::Vec(_) => todo!(),
                                    RustType::Array(_) => todo!(),
                                    RustType::Tuple(_) => todo!(),
                                    RustType::UserType(_, _) => todo!(),
                                    // RustType::MutRef(_) => todo!(),
                                    RustType::Ref(_) => todo!(),
                                    RustType::Fn(_, _, _, _, _) => todo!(),
                                    _ => return_type,
                                }
                            }
                            get_fn_type_returns(fn_info.return_type, &new_type_params)
                        }
                        RustType::Vec(_) => todo!(),
                        _ => panic!("type can't be called"),
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
                PartialRustType::StructIdent(_, _, _, _) => {
                    let js_path = match expr {
                        JsExpr::Path(path) => path,
                        _ => todo!(),
                    };
                    (JsExpr::New(js_path, args_js_expr.clone()), rust_type)
                }
                PartialRustType::EnumVariantIdent(_, _, _, _, _) | PartialRustType::RustType(_) => {
                    (
                        JsExpr::FnCall(Box::new(expr), args_js_expr.clone()),
                        rust_type,
                    )
                }
            }
        }
        // Expr::Path(expr_path)
        //     if expr_path.path.segments.last().unwrap().ident.to_string() == "new" =>
        // {
        //     // TODO improve this code
        //     JsExpr::New(
        //         expr_path
        //             .path
        //             .segments
        //             .iter()
        //             .take(expr_path.path.segments.len() - 1)
        //             .map(|seg| seg.ident.to_string())
        //             .collect::<Vec<_>>(),
        //         args,
        //     )
        // }

        // TODO Can we remove Some and just treat Some as any value vs None which is null?
        // Expr::Path(expr_path)
        //     if expr_path.path.segments.len() == 1
        //         && expr_path.path.segments[0].ident.to_string() == "Some" =>
        // {
        //     args.into_iter().next().unwrap()
        // }
        _ => (
            JsExpr::FnCall(
                Box::new(handle_expr(&*expr_call.func, global_data, current_module).0),
                args_js_expr,
            ),
            RustType::Todo,
        ),
    }
}

fn handle_expr_path(
    expr_path: &ExprPath,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
    is_having_mut_ref_taken: bool,
    // is_call: bool,
) -> (JsExpr, PartialRustType) {
    handle_expr_path_inner(
        &expr_path.path,
        global_data,
        current_module,
        is_having_mut_ref_taken,
    )
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
    current_module: &Vec<String>,
    is_having_mut_ref_taken: bool,
    // is_call: bool,
) -> (JsExpr, PartialRustType) {
    let path_idents = expr_path
        .segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect::<Vec<_>>();

    let span = debug_span!("handle_expr_path", expr_path = ?quote! { #expr_path }.to_string());
    let _guard = span.enter();

    let module = global_data
        .modules
        .iter()
        .find(|module| &module.path == current_module)
        .unwrap();

    // TODO I think a lot of this messing around with CamelCase and being hard to fix is because we should be storing the namespaced item names as Vecs intead of foo__Bar, until they are rendered to JS

    // IMPORTANT TODO
    // What is all this doing? We just got the module path to an item using `get_path()` so we should be looking up the definition of found item and then if there is remaining segments in the expr_path after then item then determine if these are an enum variant or associated fn
    // What does get_path() return if the expr_path is just a scoped variable?
    let segs_copy = expr_path
        .segments
        .iter()
        .map(|seg| RustPathSegment {
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
    // dbg!(&segs_copy);
    let (segs_copy_module_path, segs_copy_item_path, segs_copy_item_scope) = get_path(
        // By definition handle_expr_path is always handling *expressions* so want to look for scoped vars
        true,
        true,
        true,
        segs_copy,
        global_data,
        current_module,
        current_module,
        &global_data.scope_id_as_option(),
    );
    // dbg!(&segs_copy_module_path);
    // dbg!(&segs_copy_item_scope);
    // dbg!(&segs_copy_item_path);

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
    let (partial_rust_type, is_mut_var) = if segs_copy_item_path.len() == 1 {
        // TODO IMPORTANT needs to look/iterate through the static scopes, and var scope in unison, because they can shadow each other.
        // Is path a variable?
        let mut temp_scope_id = global_data.scope_id.clone();
        let mut scopes = Vec::new();
        while !temp_scope_id.is_empty() {
            let var_scope = global_data
                .scopes
                .iter()
                .find(|s| s.scope_id == temp_scope_id)
                .unwrap()
                .clone();
            let static_scope = module
                .scoped_various_definitions
                .iter()
                .find(|svd| svd.0 == temp_scope_id)
                .unwrap()
                .clone();
            scopes.push((static_scope, var_scope));
            temp_scope_id.pop();
        }

        let scoped_partial_rust_type = scopes.iter().find_map(|(static_scope, var_scope)| {
            let var = var_scope
                .variables
                .iter()
                .find(|v| v.name == item_path_seg.ident);
            let fn_info = static_scope
                .1
                .fn_info
                .iter()
                .find(|fn_info| fn_info.ident == item_path_seg.ident);
            let item_def = static_scope
                .1
                .item_definitons
                .iter()
                .find(|se| se.ident == item_path_seg.ident);
            let const_def = static_scope
                .1
                .consts
                .iter()
                .find(|const_def| const_def.name == item_path_seg.ident);

            if var.is_some() || fn_info.is_some() || item_def.is_some() || const_def.is_some() {
                Some(found_item_to_partial_rust_type(
                    &item_path_seg,
                    var,
                    fn_info,
                    item_def,
                    const_def,
                    segs_copy_module_path.clone(),
                    segs_copy_item_scope.clone(),
                ))
            } else {
                None
            }
        });

        // TODO handle len=1 enums like Some(5), None, etc
        let final_partial_rust_type =
            if let Some(scoped_partial_rust_type) = scoped_partial_rust_type {
                scoped_partial_rust_type
            } else {
                // We don't have a scoped match so path must be a module level definiton
                let item_module = global_data
                    .modules
                    .iter()
                    .find(|module| &module.path == &segs_copy_module_path)
                    .unwrap();
                let func = item_module
                    .fn_info
                    .iter()
                    .find(|se| se.ident == item_path_seg.ident);
                let item_def = item_module
                    .item_definitons
                    .iter()
                    .find(|se| se.ident == item_path_seg.ident);
                let const_def = item_module
                    .consts
                    .iter()
                    .find(|const_def| const_def.name == item_path_seg.ident);

                found_item_to_partial_rust_type(
                    &item_path_seg,
                    None,
                    func,
                    item_def,
                    const_def,
                    segs_copy_module_path.clone(),
                    segs_copy_item_scope.clone(),
                )
            };
        final_partial_rust_type
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

        // Get struct/enum item definition
        // dbg!("here");
        // dbg!(&segs_copy_module_path);
        // dbg!(&segs_copy_item_scope);
        // dbg!(&item_path_seg.ident);
        // dbg!(&sub_path.ident);
        let item_def = global_data.lookup_item_def_known_module_assert_not_func2(
            &segs_copy_module_path,
            &segs_copy_item_scope,
            &item_path_seg.ident,
        );
        // dbg!(&item_def);

        // If turbofish exists on item path segment then use that for type params, otherwise use the unresolved params defined on the item definition
        let item_generics = if item_path_seg.turbofish.len() > 0 {
            item_path_seg
                .turbofish
                .iter()
                .enumerate()
                .map(|(i, g)| RustTypeParam {
                    name: item_def.generics[i].clone(),
                    type_: RustTypeParamValue::RustType(Box::new(g.clone())),
                })
                .collect::<Vec<_>>()
        } else {
            // NOTE for now we are assuming turbofish must exist for generic items, until we implement a solution for getting type params that are resolved later in the code
            assert!(item_def.generics.len() == 0);
            item_def
                .generics
                .iter()
                .map(|g| RustTypeParam {
                    name: g.clone(),
                    type_: RustTypeParamValue::Unresolved,
                })
                .collect::<Vec<_>>()
        };

        // TODO don't like returning an Option here, should probably follow how rust does which I believe is to see if it is an enum variant first else it must be an associated fn, else panic
        let impl_method = global_data.lookup_associated_fn(
            &item_generics,
            &segs_copy_module_path,
            &segs_copy_item_scope,
            &sub_path,
            &item_path_seg.ident,
            &item_def,
        );
        // dbg!(&impl_method);
        let impl_method = impl_method.map(|impl_method| PartialRustType::RustType(impl_method));

        let enum_variant = match item_def.struct_or_enum_info {
            // Item is struct so we need to look up associated fn
            StructOrEnumDefitionInfo::Struct(struct_definition_info) => None,
            StructOrEnumDefitionInfo::Enum(enum_definition_info) => {
                // Check if we have a variant of the enum
                let enum_variant = enum_definition_info
                    .members
                    .iter()
                    .find(|member| member.ident == sub_path.ident);

                let enum_variant_generics = if sub_path.turbofish.len() > 0 {
                    sub_path
                        .turbofish
                        .iter()
                        .enumerate()
                        .map(|(i, g)| RustTypeParam {
                            name: item_def.generics[i].clone(),
                            type_: RustTypeParamValue::RustType(Box::new(g.clone())),
                        })
                        .collect::<Vec<_>>()
                } else {
                    item_def
                        .generics
                        .iter()
                        .map(|g| RustTypeParam {
                            name: g.clone(),
                            type_: RustTypeParamValue::Unresolved,
                        })
                        .collect::<Vec<_>>()
                };

                let mut enum_generics = Vec::new();
                // An enum variant instantiation cannot have turbofish on both the enum and the variant
                if item_path_seg.turbofish.len() > 0 {
                    assert!(sub_path.turbofish.len() > 0);
                    enum_generics = item_generics;
                }
                if sub_path.turbofish.len() > 0 {
                    assert!(item_path_seg.turbofish.len() > 0);
                    enum_generics = enum_variant_generics;
                }
                // NOTE for now we are assuming turbofish must exist for generic items, until we implement a solution for getting type params that are resolved later in the code
                if item_def.generics.len() > 0 {
                    assert!(enum_generics.len() > 0);
                }

                enum_variant.map(|enum_variant| {
                    if enum_variant.inputs.len() == 0 {
                        PartialRustType::RustType(RustType::StructOrEnum(
                            enum_generics,
                            segs_copy_module_path.clone(),
                            segs_copy_item_scope.clone(),
                            item_def.ident,
                        ))
                    } else {
                        PartialRustType::EnumVariantIdent(
                            enum_generics,
                            segs_copy_module_path.clone(),
                            segs_copy_item_scope.clone(),
                            item_def.ident,
                            sub_path.ident.clone(),
                        )
                    }
                })
            }
        };
        // If you have an enum variant and associated fn with the same name, the code will compile, but if you try to access the fn you will just get the variant instead
        if let Some(enum_variant) = enum_variant {
            (enum_variant, false)
        } else if let Some(impl_method) = impl_method {
            (impl_method, false)
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
    if is_having_mut_ref_taken && !is_mut_var {
        panic!("a mutable reference can only be taken of a mutable var");
    }
    let is_mut_ref_js_primative = match &partial_rust_type {
        PartialRustType::RustType(rust_type) => match rust_type {
            RustType::MutRef(rust_type2) => rust_type2.is_js_primative(),
            _ => false,
        },
        _ => false,
    };

    // Make JS path
    // segs_copy_module_path, segs_copy_item_path, is_scoped
    // Lookup module path to find what it's deduplicated name is
    // Check whether it is not globally unique and so has been namespaced
    // dbg!(&segs_copy_item_path);
    let mut js_segs = segs_copy_item_path
        .iter()
        .map(|seg| case_convert(seg.ident.to_string()))
        .collect::<Vec<_>>();
    // dbg!(&js_segs);

    if segs_copy_module_path == &["web_prelude"] && segs_copy_item_path[0].ident == "Document" {
        js_segs[0] = "document".to_string();
    }

    // TODO Surely this should be `.is_none()`?
    // if segs_copy_item_scope.is_some() {
    if segs_copy_item_scope.is_none() {
        if let Some(dup) = global_data.duplicates.iter().find(|dup| {
            dup.name == segs_copy_item_path[0].ident
                && &dup.original_module_path == &segs_copy_module_path
        }) {
            js_segs[0] = dup
                .namespace
                .iter()
                .map(|seg| case_convert(seg))
                .collect::<Vec<_>>()
                .join("__");
        }
    } else {
        if js_segs[0] == "self" {
            js_segs[0] = "this".to_string();
        }
    }
    // dbg!(&js_segs);

    // let segs = segs_copy.iter()
    // TODO all this logic could be cleaned up and/or made clearer
    let final_expr = if is_mut_ref_js_primative || is_having_mut_ref_taken || !is_mut_var {
        JsExpr::Path(js_segs)
    } else {
        match &partial_rust_type {
            PartialRustType::StructIdent(_, _, _, _) => todo!(),
            PartialRustType::EnumVariantIdent(_, _, _, _, _) => todo!(),
            PartialRustType::RustType(rust_type) => {
                if rust_type.is_js_primative() {
                    JsExpr::Field(Box::new(JsExpr::Path(js_segs)), "inner".to_string())
                } else {
                    // TODO Need to .copy() for non-primative types, and check they are `Copy` else panic because they would need to be cloned?
                    JsExpr::Path(js_segs)
                }
            }
        }
    };
    (final_expr, partial_rust_type)
}

/// Get match pattern ident to be used as rhs of if conditions like `myData.id === MyEnum.fooId`, and start a body stmts Vec to with any pattern arg destructuring that might be necessary
///
/// (rhs, assignments/destructuring at start of body Vec)
fn handle_match_pat(
    arm_pat: &Pat,
    expr_match: &ExprMatch,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
    match_condition_type: &RustType,
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
                    Member::Named(ident) => DestructureValue::KeyName(ident.to_string()),
                    Member::Unnamed(_) => todo!(),
                })
                .collect::<Vec<_>>();

            // A struct pattern in a match arm always implies an enum? if so, look up enum to find types of struct patterns
            let enum_def_info = match match_condition_type {
                RustType::StructOrEnum(type_params, module_path, scope_id, name) => {
                    let item_def = global_data.lookup_item_def_known_module_assert_not_func2(
                        module_path,
                        scope_id,
                        name,
                    );
                    match item_def.struct_or_enum_info {
                        StructOrEnumDefitionInfo::Struct(_) => todo!(),
                        StructOrEnumDefitionInfo::Enum(enum_def_info) => enum_def_info,
                    }
                }
                _ => todo!(),
            };
            let arm_field_defs = enum_def_info
                .members
                .iter()
                .find(|member| {
                    member.ident == pat_struct.path.segments.last().unwrap().ident.to_string()
                })
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
                            type_: field_type,
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
                    Box::new(handle_expr(&*expr_match.expr, global_data, current_module).0),
                    "data".to_string(),
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
            let names = pat_tuple_struct
                .elems
                .iter()
                .map(|elem| handle_pat(elem, global_data, RustType::I32))
                .collect::<Vec<_>>();

            // A TupleStruct pattern in a match arm always implies an enum? if so, look up enum to find types of struct patterns
            let enum_def_info = match match_condition_type {
                RustType::StructOrEnum(type_params, module_path, scope_id, name) => {
                    let item_def = global_data.lookup_item_def_known_module_assert_not_func2(
                        module_path,
                        scope_id,
                        name,
                    );
                    match item_def.struct_or_enum_info {
                        StructOrEnumDefitionInfo::Struct(_) => todo!(),
                        StructOrEnumDefitionInfo::Enum(enum_def_info) => enum_def_info,
                    }
                }
                _ => todo!(),
            };
            let arm_field_defs = enum_def_info
                .members
                .iter()
                .find(|member| {
                    member.ident
                        == pat_tuple_struct
                            .path
                            .segments
                            .last()
                            .unwrap()
                            .ident
                            .to_string()
                })
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
                    let field_type = match &arm_field_defs.inputs[i] {
                        EnumVariantInputsInfo::Named { ident, input_type } => todo!(),
                        EnumVariantInputsInfo::Unnamed(input_type) => input_type.clone(),
                    };

                    ScopedVar {
                        name,
                        mut_: false,
                        // mut_ref: false,
                        type_: field_type,
                    }
                })
                .collect::<Vec<_>>();
            let stmt = JsStmt::Local(JsLocal {
                public: false,
                export: false,
                type_: LocalType::Var,
                lhs: LocalName::DestructureArray(names),
                value: JsExpr::Field(
                    Box::new(handle_expr(&*expr_match.expr, global_data, current_module).0),
                    "data".to_string(),
                ),
            });
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
    expr_match: &ExprMatch,
    is_returned: bool,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> (JsExpr, RustType) {
    // (assignment, condition, succeed, fail)
    // TODO we need to know whether match result is being assigned to a var and therefore the if statement should be adding assignments to the end of each block
    debug_span!("handle_expr_match", expr_match = ?quote! { #expr_match }.to_string());

    let (match_condition_expr, match_condition_type) =
        handle_expr(&*expr_match.expr, global_data, current_module);

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
            let (mut cond_rhs, mut body_data_destructure, scoped_vars) = handle_match_pat(
                &arm.pat,
                expr_match,
                global_data,
                current_module,
                &match_condition_type,
            );

            if cond_rhs == ["Some"] || cond_rhs == ["None"] {
                cond_rhs.insert(0, "Option".to_string());
            }

            // Need to take the path which will be eg [MyEnum, Baz], and convert to [MyEnum.bazId]
            let index = cond_rhs.len() - 1;
            // dbg!(rhs);
            // todo!();
            // if rhs[0] == "Option" {
            //     rhs = rhs[1..].to_vec();
            // }
            cond_rhs[index] = format!("{}Id", camel(cond_rhs[index].clone()));

            // Create new scope for match arm block
            // NOTE even if there is no curly braces it is still a scope
            global_data.push_new_scope(true, scoped_vars);

            let (body_js_stmts, body_return_type) = match &*arm.body {
                // Expr::Array(_) => [JsStmt::Raw("sdafasdf".to_string())].to_vec(),
                // TODO not sure what this is for???
                Expr::Array(_) => (vec![JsStmt::Raw("sdafasdf".to_string())], RustType::Todo),
                Expr::Block(expr_block) => {
                    // TODO should probably be using handle_body() or something for this
                    let (js_stmts, types_): (Vec<_>, Vec<_>) = expr_block
                        .block
                        .stmts
                        .iter()
                        .map(|stmt| handle_stmt(stmt, global_data, current_module))
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
            global_data.pop_scope();

            body_data_destructure.extend(body_js_stmts.into_iter());
            let body = body_data_destructure;

            let (prev_if_expr, prev_body_return_type) = acc;
            (
                JsExpr::If(JsIf {
                    assignment: is_returned
                        .then_some(LocalName::Single("ifTempAssignment".to_string())),
                    declare_var: is_returned,
                    condition: Box::new(JsExpr::Binary(
                        Box::new(JsExpr::Field(
                            Box::new(match_condition_expr.clone()),
                            "id".to_string(),
                        )),
                        JsOp::Eq,
                        Box::new(JsExpr::Path(cond_rhs)),
                    )),
                    succeed: body,
                    // TODO
                    fail: Some(Box::new(prev_if_expr)),
                }),
                match prev_body_return_type {
                    Some(prev_body_return_type) => Some(match prev_body_return_type {
                        RustType::NotAllowed => todo!(),
                        RustType::Unknown => todo!(),
                        RustType::Todo => todo!(),
                        RustType::ParentItem => todo!(),
                        RustType::Unit => todo!(),
                        RustType::Never => body_return_type,
                        RustType::ImplTrait(_) => todo!(),
                        RustType::TypeParam(_) => todo!(),
                        RustType::I32 => prev_body_return_type,
                        RustType::F32 => prev_body_return_type,
                        RustType::Bool => prev_body_return_type,
                        RustType::String => prev_body_return_type,
                        RustType::Option(_) => todo!(),
                        RustType::Result(_) => todo!(),
                        RustType::StructOrEnum(_, _, _, _) => prev_body_return_type,
                        RustType::Vec(_) => prev_body_return_type,
                        RustType::Array(_) => prev_body_return_type,
                        RustType::Tuple(_) => prev_body_return_type,
                        RustType::UserType(_, _) => todo!(),
                        RustType::MutRef(_) => todo!(),
                        RustType::Ref(_) => prev_body_return_type,
                        RustType::Fn(_, _, _, _, _) => todo!(),
                        RustType::Closure(_) => todo!(),
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
