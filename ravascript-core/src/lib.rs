use biome_formatter::{FormatLanguage, IndentStyle, IndentWidth};
use biome_js_formatter::{context::JsFormatOptions, JsFormatLanguage};
use biome_js_parser::JsParserOptions;
use biome_js_syntax::JsFileSource;
use heck::{AsKebabCase, AsLowerCamelCase, AsPascalCase};
use std::{
    fmt::Debug,
    fs,
    net::ToSocketAddrs,
    path::{Path, PathBuf},
};
use syn::{
    parse_macro_input, BinOp, DeriveInput, Expr, ExprBlock, ExprMatch, Fields, FnArg, ImplItem,
    Item, ItemEnum, ItemFn, ItemImpl, ItemMod, ItemStruct, ItemTrait, ItemUse, Lit, Member, Meta,
    Pat, ReturnType, Stmt, TraitItem, Type, UnOp, UseTree, Visibility,
};
pub mod prelude;
pub mod rust_prelude;

// TODO need to handle expressions which return `()`. Probably use `undefined` for `()` since that is what eg console.log();, var x = 5;, etc returns;
// TODO preserve new lines so generated js is more readable
// TODO consider how to get RA/cargo check to analyze rust inputs in `testing/`
// TODO add assertions to output JS and run that JS to ensure assertions pass

// fn handle_item_use_tree(use_tree: &UseTree, sub_modules: &mut Vec<DestructureValue>) {
//     dbg!(use_tree);
//     match use_tree {
//         UseTree::Path(use_path) => {
//             let key = use_path.ident.to_string();
//             let value = handle_item_use_tree(&*use_path.tree, sub_modules, root_module_or_crate);
//             sub_modules.push(Des)
//         }
//         UseTree::Name(use_name) => sub_modules.push(use_name.ident.to_string()),
//         UseTree::Rename(_) => todo!(),
//         UseTree::Glob(_use_glob) => sub_modules.push("*".to_string()),
//         UseTree::Group(use_group) => use_group.items.iter().for_each(|item| match item {
//             UseTree::Path(_) => todo!(),
//             UseTree::Name(use_name) => sub_modules.push(use_name.ident.to_string()),
//             UseTree::Rename(_) => todo!(),
//             UseTree::Glob(_) => todo!(),
//             UseTree::Group(_) => todo!(),
//         }),
//     }
// }

fn tree_to_destructure_object(use_tree: &UseTree) -> DestructureObject {
    match use_tree {
        UseTree::Path(use_path) => DestructureObject(vec![DestructureValue::Nesting(
            case_convert(&use_path.ident),
            tree_to_destructure_object(&*use_path.tree),
        )]),
        UseTree::Name(use_name) => DestructureObject(vec![DestructureValue::KeyName(
            case_convert(&use_name.ident),
        )]),
        UseTree::Rename(_) => todo!(),
        UseTree::Glob(_) => DestructureObject(vec![]),
        UseTree::Group(use_group) => DestructureObject(
            use_group
                .items
                .iter()
                .map(|item| match item {
                    UseTree::Path(use_path) => DestructureValue::Nesting(
                        case_convert(&use_path.ident),
                        tree_to_destructure_object(&*use_path.tree),
                    ),
                    UseTree::Name(use_name) => {
                        DestructureValue::KeyName(case_convert(&use_name.ident))
                    }
                    UseTree::Rename(_) => todo!(),
                    UseTree::Glob(_) => todo!(),
                    UseTree::Group(_) => todo!(),
                })
                .collect::<Vec<_>>(),
        ),
    }
}

/// We want each of used items eg `use mod::sub_mod::{item1, item2, another_mod::item3}` to return the name of the item, and the path relative to the root module
///
/// relative_path (snake) is a temporary var for building the relative path
///
/// items is what gets stored in global_data  Vec<(item name (snake), relative path (snake))>
fn tree_parsing_for_boilerplate(
    use_tree: &UseTree,
    relative_path: &mut Vec<String>,
    items: &mut Vec<(String, Vec<String>)>,
) {
    match use_tree {
        UseTree::Path(use_path) => {
            relative_path.push(use_path.ident.to_string());
            tree_parsing_for_boilerplate(&*use_path.tree, relative_path, items);
        }
        UseTree::Name(use_name) => items.push((use_name.ident.to_string(), relative_path.clone())),
        UseTree::Rename(_) => todo!(),
        UseTree::Glob(_) => todo!(),
        UseTree::Group(use_group) => {
            for item in &use_group.items {
                match item {
                    UseTree::Path(use_path) => {
                        let mut new_relative_path = relative_path.clone();
                        new_relative_path.push(use_path.ident.to_string());
                        tree_parsing_for_boilerplate(
                            &*use_path.tree,
                            &mut new_relative_path,
                            items,
                        );
                    }
                    UseTree::Name(use_name) => {
                        items.push((use_name.ident.to_string(), relative_path.clone()));
                    }
                    UseTree::Rename(_) => todo!(),
                    UseTree::Glob(_) => todo!(),
                    UseTree::Group(_) => todo!(),
                }
            }
        }
    };
}

/// CONST_NAMES -> CONST_NAMES
/// PascalCase -> PascalCase
/// snake_case -> snakeCase
fn case_convert(name: impl ToString) -> String {
    let name = name.to_string();

    if name.chars().all(|c| c.is_uppercase() || c == '_') {
        // NOTE JS seems to be okay with uppercase keywords so don't need to convert
        name
    } else if name.chars().next().unwrap().is_ascii_uppercase() {
        // TODO this is redundant?
        AsPascalCase(name).to_string()
    } else {
        camel(name)
    }
}

fn handle_item_use(
    item_use: &ItemUse,
    current_module: Vec<String>,
    is_module: bool,
    // global_data: &mut GlobalData,
    modules: &mut Vec<ModuleData>,
) {
    let public = match item_use.vis {
        Visibility::Public(_) => true,
        _ => false,
    };

    let (root_module_or_crate, sub_modules) = match &item_use.tree {
        UseTree::Path(use_path) => {
            // let mut sub_modules: Vec<DestructureValue> = Vec::new();
            let root_module_or_crate = use_path.ident.to_string();

            let sub_modules = tree_to_destructure_object(&*use_path.tree);
            // let sub_modules = DestructureObject (sub_modules);
            // handle_item_use_tree(&*use_path.tree, &mut sub_modules),
            (root_module_or_crate, sub_modules)
        }
        // UseTree::Name(use_name) => sub_modules.push(use_name.ident.to_string()),
        // TODO need to consider what a simple `use foo` means, since for modules this would be preceeded by `mod foo` which has the same effect?
        UseTree::Name(use_name) => todo!(),
        _ => panic!("root of use trees are always a path or name"),
    };

    // handle globs
    if sub_modules.0.len() == 0 {
        // For now we are not handling globs but need to use them for using enum variants which we will then need to inject manually
        return;
    }

    if root_module_or_crate == "ravascript" || root_module_or_crate == "crate" {
        match &sub_modules.0[0] {
            DestructureValue::KeyName(_) => {}
            DestructureValue::Rename(_, _) => {}
            DestructureValue::Nesting(name, _) => {
                if name == "prelude" {
                    return;
                }
            }
        }
    }

    // TODO fix this mess
    if match sub_modules.0.first().unwrap() {
        DestructureValue::KeyName(_) => "",
        DestructureValue::Rename(_, _) => "",
        DestructureValue::Nesting(name, _) => name,
    } == "web"
    {
        if root_module_or_crate == "Sse" {
            // JsStmt::Raw(SSE_RAW_FUNC.to_string())
        } else {
            // JsStmt::Expr(JsExpr::Vanish, false)
        }
    } else if root_module_or_crate == "serde" || root_module_or_crate == "serde_json" {
        // JsStmt::Expr(JsExpr::Vanish, false)
    } else if root_module_or_crate == "crate" {
        // If we import something from our crate, inline it (probably what we want for external crates too?)
        // A much simpler plan for now is to force defining the type in the JS file, and then export, rather than the other way round
        // Get the name of the item to be inlined
        todo!()
    } else {
        // JsStmt::Local(JsLocal {
        //     public,
        //     export: false,
        //     type_: LocalType::Var,
        //     lhs: LocalName::DestructureObject(sub_modules),
        //     value: JsExpr::Path(vec![camel(root_module_or_crate)]),
        // })

        // TODO we do want to do the JsLocal destructure thing if the use is not a top level item?
        // TODO this is probably also the correct place to determine if std stuff like HashMap needs flagging
        if is_module {
            // eg this.colorModule.spinachMessage = this.colorModule.greenModule.spinachModule.spinachMessage;

            let (_root_module_or_crate, item_paths) = match &item_use.tree {
                UseTree::Path(use_path) => {
                    // let mut sub_modules: Vec<DestructureValue> = Vec::new();
                    let root_module_or_crate = use_path.ident.to_string();

                    // Vec<(item name (snake), relative path (snake))>
                    let mut item_paths = Vec::new();
                    let mut relative_path = vec![use_path.ident.to_string()];
                    tree_parsing_for_boilerplate(
                        &*use_path.tree,
                        &mut relative_path,
                        &mut item_paths,
                    );
                    // let sub_modules = DestructureObject (sub_modules);
                    // handle_item_use_tree(&*use_path.tree, &mut sub_modules),
                    (root_module_or_crate, item_paths)
                }
                // UseTree::Name(use_name) => sub_modules.push(use_name.ident.to_string()),
                // TODO need to consider what a simple `use foo` means, since for modules this would be preceeded by `mod foo` which has the same effect?
                UseTree::Name(_use_name) => todo!(),
                _ => panic!("root of use trees are always a path or name"),
            };

            let module = modules
                .iter_mut()
                .find(|module| module.path == current_module)
                .unwrap();
            for item_path in item_paths {
                // Get current module since it must already exist if we are in it
                match item_use.vis {
                    Visibility::Public(_) => module.pub_use_mappings.push(item_path),
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => module.private_use_mappings.push(item_path),
                }
            }
        }
    }
}

fn camel(text: impl ToString) -> String {
    let text = text.to_string();

    // rename JavaScript keywords
    let text = if text == "default" {
        "default_vzxyw".to_string()
    } else {
        text
    };

    let underscore_prefix = text.starts_with("_");
    let camel = AsLowerCamelCase(text).to_string();
    if underscore_prefix {
        format!("_{camel}")
    } else {
        camel
    }
}

fn handle_destructure_pat(pat: &Pat, member: &Member) -> DestructureValue {
    match pat {
        Pat::Const(_) => todo!(),
        Pat::Ident(pat_ident) => {
            let pat_ident = pat_ident.ident.to_string();
            let member = match member {
                Member::Named(ident) => ident.to_string(),
                Member::Unnamed(_) => todo!(),
            };
            if member == pat_ident {
                DestructureValue::KeyName(camel(member))
            } else {
                DestructureValue::Rename(camel(member), camel(pat_ident))
            }
        }
        Pat::Lit(_) => todo!(),
        Pat::Macro(_) => todo!(),
        Pat::Or(_) => todo!(),
        Pat::Paren(_) => todo!(),
        Pat::Path(_) => todo!(),
        Pat::Range(_) => todo!(),
        Pat::Reference(_) => todo!(),
        Pat::Rest(_) => todo!(),
        Pat::Slice(_) => todo!(),
        Pat::Struct(pat_struct) => {
            let member = match member {
                Member::Named(ident) => ident.to_string(),
                Member::Unnamed(_) => todo!(),
            };
            let fields = pat_struct
                .fields
                .iter()
                .map(|field| handle_destructure_pat(&field.pat, &field.member))
                .collect::<Vec<_>>();
            DestructureValue::Nesting(camel(member), DestructureObject(fields))
        }
        Pat::Tuple(_) => todo!(),
        Pat::TupleStruct(_) => todo!(),
        Pat::Type(_) => todo!(),
        Pat::Verbatim(_) => todo!(),
        Pat::Wild(_) => todo!(),
        _ => todo!(),
    }
}
fn handle_pat(pat: &Pat) -> LocalName {
    match pat {
        Pat::Const(_) => todo!(),
        Pat::Ident(pat_ident) => LocalName::Single(camel(&pat_ident.ident)),
        Pat::Lit(_) => todo!(),
        Pat::Macro(_) => todo!(),
        Pat::Or(_) => todo!(),
        Pat::Paren(_) => todo!(),
        Pat::Path(_) => todo!(),
        Pat::Range(_) => todo!(),
        Pat::Reference(_) => todo!(),
        Pat::Rest(_) => todo!(),
        Pat::Slice(pat_slice) => LocalName::DestructureArray(
            pat_slice
                .elems
                .iter()
                .map(|elem| handle_pat(elem))
                .collect::<Vec<_>>(),
        ),
        Pat::Struct(pat_struct) => {
            let fields = pat_struct
                .fields
                .iter()
                .map(|field| handle_destructure_pat(&field.pat, &field.member))
                .collect::<Vec<_>>();
            LocalName::DestructureObject(DestructureObject(fields))
        }
        Pat::Tuple(pat_tuple) => LocalName::DestructureArray(
            pat_tuple
                .elems
                .iter()
                .map(|elem| handle_pat(elem))
                .collect::<Vec<_>>(),
        ),
        Pat::TupleStruct(_) => todo!(),
        Pat::Type(pat_type) => handle_pat(&pat_type.pat),
        Pat::Verbatim(_) => todo!(),
        // for `let _ = foo();` the lhs will be `Pat::Wild`
        Pat::Wild(_) => LocalName::Single("_".to_string()),
        other => {
            dbg!(other);
            todo!();
        }
    }
}

fn handle_stmt(
    stmt: &Stmt,
    global_data: &mut GlobalData,
    current_module_path: &Vec<String>,
) -> JsStmt {
    match stmt {
        Stmt::Expr(expr, closing_semi) => {
            //
            let mut js_expr = handle_expr(expr, global_data, current_module_path);
            if should_copy_expr_unary(expr, global_data) {
                js_expr = JsExpr::MethodCall(Box::new(js_expr), "copy".to_string(), Vec::new());
            }

            JsStmt::Expr(js_expr, closing_semi.is_some())
        }
        Stmt::Local(local) => {
            let lhs = handle_pat(&local.pat);

            // TODO should also check:
            // by_ref: Some(
            //     Ref,
            // ),
            // mutability: Some(
            //     Mut,
            // ),
            let is_mut_ref = match &*local.init.as_ref().unwrap().expr {
                Expr::Reference(expr_ref) => expr_ref.mutability.is_some(),
                _ => false,
            };
            let mut rhs = handle_expr(
                &*local.init.as_ref().unwrap().expr,
                global_data,
                current_module_path,
            );

            // Add .copy() if rhs is a mut...
            // and rhs is `Copy`
            let rhs_is_mut = global_data.scopes.last().unwrap().0.iter().rev().find(
                |ScopedVar { name, mut_, .. }| {
                    *mut_
                        && match &*local.init.as_ref().unwrap().expr {
                            Expr::Path(expr_path) => {
                                if expr_path.path.segments.len() == 1 {
                                    expr_path.path.segments.first().unwrap().ident == name
                                } else {
                                    false
                                }
                            }
                            _ => false,
                        }
                },
            );

            // TODO
            // let rhs_is_deref_mut_ref = ...

            let lhs_is_mut = match &local.pat {
                Pat::Ident(pat_ident) => pat_ident.mutability.is_some(),
                _ => false,
            };
            let rhs_is_var = match &*local.init.as_ref().unwrap().expr {
                Expr::Path(expr_path) => expr_path.path.segments.len() == 1,
                _ => false,
            };

            // copy rhs if is mut and is a variable, which is being assigned
            if let Some(rhs_is_mut) = rhs_is_mut {
                rhs = JsExpr::MethodCall(Box::new(rhs), "copy".to_string(), vec![]);

            // Also copy if we are assigning *a variable* (not a literal) to a mutable variable (eg the rhs hand side is immutable so we didn't care about copying, but now that it is being assigned to another var, we need to copy to make sure that mutating the new var doesn't update the original var)
            } else if lhs_is_mut && rhs_is_var {
                rhs = JsExpr::MethodCall(Box::new(rhs), "copy".to_string(), vec![]);
            } else if should_copy_expr_unary(&*local.init.as_ref().unwrap().expr, global_data) {
                rhs = JsExpr::MethodCall(Box::new(rhs), "copy".to_string(), vec![]);
            }

            dbg!(&lhs);
            dbg!(global_data.scopes.last().unwrap());
            // rhs is a fn call that returns a &mut
            let fn_call_mut_ref = match &*local.init.as_ref().unwrap().expr {
                Expr::Call(expr_call) => {
                    match &*expr_call.func {
                        Expr::Path(expr_path) => {
                            if expr_path.path.segments.len() == 1 {
                                let fn_name =
                                    expr_path.path.segments.first().unwrap().ident.to_string();
                                global_data.scopes.iter().rev().any(|(vars, item_fns)| {
                                    let item_fn = item_fns
                                        .iter()
                                        .rev()
                                        .find(|item_fn| item_fn.sig.ident.to_string() == fn_name);
                                    if let Some(item_fn) = item_fn {
                                        match &item_fn.sig.output {
                                            ReturnType::Default => false,
                                            ReturnType::Type(_, type_) => {
                                                // TODO handle other cases eg a tuple containing &mut
                                                match &**type_ {
                                                    Type::Array(_) => false,
                                                    Type::BareFn(_) => false,
                                                    Type::Group(_) => false,
                                                    Type::ImplTrait(_) => false,
                                                    Type::Infer(_) => false,
                                                    Type::Macro(_) => false,
                                                    Type::Never(_) => false,
                                                    Type::Paren(_) => false,
                                                    Type::Path(_) => false,
                                                    Type::Ptr(_) => false,
                                                    Type::Reference(type_reference) => {
                                                        type_reference.mutability.is_some()
                                                    }
                                                    Type::Slice(_) => false,
                                                    Type::TraitObject(_) => false,
                                                    Type::Tuple(_) => false,
                                                    Type::Verbatim(_) => false,
                                                    _ => false,
                                                }
                                            }
                                        }
                                    } else {
                                        false
                                    }
                                })
                            } else {
                                false
                            }
                        }
                        _ => false,
                    }
                }
                _ => false,
            };

            dbg!(fn_call_mut_ref);
            // Record name if creating a mut or &mut variable
            match &local.pat {
                Pat::Ident(pat_ident) => {
                    if is_mut_ref || pat_ident.mutability.is_some() || fn_call_mut_ref {
                        global_data.scopes.last_mut().unwrap().0.push(ScopedVar {
                            name: pat_ident.ident.to_string(),
                            mut_: pat_ident.mutability.is_some(),
                            mut_ref: is_mut_ref || fn_call_mut_ref,
                            type_: "".to_string(),
                        });
                    }
                }
                _ => {}
            }
            dbg!(global_data.scopes.last().unwrap());
            // match &lhs {
            //     LocalName::Single(var_name) => {
            //         if is_mut_ref || local.init.
            //         global_data.vars_in_scope.push((var_name, ))
            //     },
            //     // TODO handle_pat needs to capture whether destructured variables are mut
            //     LocalName::DestructureObject(_) => {}
            //     LocalName::DestructureArray(_) => {}
            // }

            match rhs {
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

                rhs => JsStmt::Local(JsLocal {
                    public: false,
                    export: false,
                    type_: LocalType::Var,
                    lhs,
                    value: rhs,
                }),
            }
        }
        Stmt::Item(item) => match item {
            // TODO this should all be handled by `fn handle_item()`
            Item::Const(_) => todo!(),
            Item::Enum(item_enum) => {
                handle_item_enum(item_enum.clone(), global_data, current_module_path)
            }
            Item::ExternCrate(_) => todo!(),
            Item::Fn(item_fn) => handle_item_fn(item_fn, global_data, current_module_path),
            Item::ForeignMod(_) => todo!(),
            Item::Impl(item_impl) => {
                handle_item_impl(item_impl, global_data, current_module_path);
                JsStmt::Expr(JsExpr::Vanish, false)
            }
            Item::Macro(_) => todo!(),
            Item::Mod(_) => todo!(),
            Item::Static(_) => todo!(),
            // Item::Struct(_) => JsStmt::Expr(JsExpr::Vanish, false),
            Item::Struct(item_struct) => {
                handle_item_struct(item_struct, global_data, current_module_path)
            }
            Item::Trait(item_trait) => {
                handle_item_trait(item_trait, global_data, current_module_path);
                JsStmt::Expr(JsExpr::Vanish, false)
            }
            Item::TraitAlias(_) => todo!(),
            Item::Type(_) => todo!(),
            Item::Union(_) => todo!(),
            // Item::Use(item_use) => handle_item_use(item_use),
            Item::Use(item_use) => todo!(),
            Item::Verbatim(_) => todo!(),
            _ => todo!(),
        },
        Stmt::Macro(stmt_macro) => {
            let path_segs = stmt_macro
                .mac
                .path
                .segments
                .iter()
                .map(|seg| seg.ident.to_string())
                .collect::<Vec<_>>();
            if path_segs.len() == 1 {
                if path_segs[0] == "panic" {
                    let input = stmt_macro.mac.tokens.clone().to_string();
                    // TODO use a custom error so it isn't inadvertently caught by other JS code in the app?
                    return JsStmt::Raw(format!(r#"throw new Error("{input}")"#));
                }
                if path_segs[0] == "try_" {
                    let input = stmt_macro.mac.tokens.clone().to_string();
                    let try_block = syn::parse_str::<syn::Block>(&input).unwrap();
                    let stmt_vec = try_block
                        .stmts
                        .iter()
                        .map(|stmt| handle_stmt(stmt, global_data, current_module_path))
                        .collect::<Vec<_>>();
                    return JsStmt::TryBlock(stmt_vec);
                }
                if path_segs[0] == "catch" {
                    let input = stmt_macro.mac.tokens.clone().to_string();
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
                        .map(|stmt| handle_stmt(&stmt, global_data, current_module_path));
                    let stmt_vec = stmt_vec.collect::<Vec<_>>();
                    return JsStmt::CatchBlock(err_var_name, stmt_vec);
                }
                if path_segs[0] == "assert" {
                    let input = stmt_macro.mac.tokens.clone().to_string();
                    let condition_expr = syn::parse_str::<syn::Expr>(&input).unwrap();

                    let condition_js = JsExpr::Field(
                        Box::new(JsExpr::Paren(Box::new(handle_expr(
                            &condition_expr,
                            global_data,
                            current_module_path,
                        )))),
                        "jsBoolean".to_string(),
                    );

                    return JsStmt::Expr(
                        JsExpr::MethodCall(
                            Box::new(JsExpr::Path(vec!["console".to_string()])),
                            "assert".to_string(),
                            vec![condition_js],
                        ),
                        stmt_macro.semi_token.is_some(),
                    );
                }
                if path_segs[0] == "assert_eq" {
                    let input = stmt_macro.mac.tokens.clone().to_string();
                    let mut parts = input.split(",");

                    let lhs = parts.next().unwrap();
                    let lhs = syn::parse_str::<syn::Expr>(lhs).unwrap();
                    let lhs = handle_expr(&lhs, global_data, current_module_path);

                    let rhs = parts.next().unwrap();
                    let rhs = syn::parse_str::<syn::Expr>(rhs).unwrap();
                    let rhs = handle_expr(&rhs, global_data, current_module_path);

                    // let equality_check = JsExpr::Binary(Box::new(lhs), JsOp::Eq, Box::new(rhs));
                    let equality_check = JsExpr::Field(
                        Box::new(JsExpr::Paren(Box::new(JsExpr::MethodCall(
                            Box::new(lhs),
                            "eq".to_string(),
                            vec![rhs],
                        )))),
                        "jsBoolean".to_string(),
                    );
                    return JsStmt::Expr(
                        JsExpr::MethodCall(
                            Box::new(JsExpr::Path(vec!["console".to_string()])),
                            "assert".to_string(),
                            vec![equality_check],
                        ),
                        stmt_macro.semi_token.is_some(),
                    );
                }
            }
            dbg!(stmt_macro);
            todo!()
        }
    }
}

fn handle_item_fn(
    item_fn: &ItemFn,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> JsStmt {
    let mut name = item_fn.sig.ident.to_string();
    dbg!(&name);

    let duplicates = &global_data.duplicates;
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

    if let Some(dup) = duplicates
        .iter()
        .find(|dup| dup.name == name && &dup.original_module_path == current_module)
    {
        name = dup
            .namespace
            .iter()
            .map(|seg| camel(seg))
            .collect::<Vec<_>>()
            .join("__");
    } else {
        name = camel(name);
    }

    // Record this fn in the *parent* scope
    global_data
        .scopes
        .last_mut()
        .unwrap()
        .1
        .push(item_fn.clone());

    // Create new scope for fn vars
    global_data.scopes.push((Vec::new(), Vec::new()));

    // record which vars are mut and/or &mut
    let mut copy_stmts = Vec::new();
    for input in &item_fn.sig.inputs {
        match input {
            FnArg::Receiver(_) => {}
            FnArg::Typed(pat_type) => match &*pat_type.pat {
                Pat::Ident(pat_ident) => {
                    let mut_ref = match &*pat_type.ty {
                        Type::Reference(type_reference) => type_reference.mutability.is_some(),
                        _ => false,
                    };

                    if pat_ident.mutability.is_some() || mut_ref {
                        global_data.scopes.last_mut().unwrap().0.push(ScopedVar {
                            name: pat_ident.ident.to_string(),
                            mut_: pat_ident.mutability.is_some(),
                            mut_ref,
                            type_: "".to_string(),
                        })
                    }

                    // record var name if mut or &mut
                    if pat_ident.mutability.is_some() {
                        copy_stmts.push(JsStmt::Local(JsLocal {
                            public: false,
                            export: false,
                            type_: LocalType::Var,
                            lhs: LocalName::Single(pat_ident.ident.to_string()),
                            value: JsExpr::MethodCall(
                                Box::new(JsExpr::Path(vec![pat_ident.ident.to_string()])),
                                "copy".to_string(),
                                Vec::new(),
                            ),
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
        copy_stmts.extend(
            item_fn
                .block
                .stmts
                .iter()
                .map(|stmt| handle_stmt(stmt, global_data, current_module)),
        );
        let iife = item_fn.sig.ident == "main";
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
            name,
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
    global_data.scopes.pop();

    stmt
}

/// We convert enum variants like Foo::Bar to Foo.bar because otherwise when the variant has arguments, syn is not able to distinguish it from an associated method, so we cannot deduce when Pascal or Camel case should be used, so stick to Pascal for both case.
/// We must store separate <variant name>Id fields because otherwise we end up in a situation where a variable containing an enum variant only contains the data returned the the method with that name and then we can't do myVariantVar === MyEnum::Variant because the lhs is data and the rhs is a function.
fn handle_item_enum(
    item_enum: ItemEnum,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> JsStmt {
    // dbg!(item_enum.attrs);

    let mut class_name = item_enum.ident.to_string();

    let mut methods = Vec::new();
    let body_stmts = vec![
        JsStmt::Raw("this.id = id;".to_string()),
        JsStmt::Raw("this.data = data;".to_string()),
    ];
    methods.push((
        item_enum.ident.to_string(),
        false,
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
                false,
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
    })
}

fn handle_item_impl(
    item_impl: &ItemImpl,
    global_data: &mut GlobalData,
    current_module_path: &Vec<String>,
) {
    // impls seem to be basically "hoisted", eg even placed in an unreachable branch, the method is still available on the original item
    // fn main() {
    //     struct Cool {}
    //     if false {
    //         fn inner() {
    //             impl Cool {
    //                 fn whatever(&self) {
    //                     dbg!("hi");
    //                 }
    //             }
    //         }
    //     }
    //     let cool = Cool {};
    //     cool.whatever();
    // }
    // [src/main.rs:8] "hi" = "hi"

    // Where different impls with the same name in different branches is considered "duplicate definitions". similarly different impls in different modules also causes a duplication error eg:
    // fn main() {
    //     struct Cool {}
    //     if false {
    //         fn inner() {
    //             impl Cool {
    //                 fn whatever(&self) {
    //                     dbg!("hi");
    //                 }
    //             }
    //         }
    //     } else {
    //         fn inner() {
    //             impl Cool {
    //                 fn whatever(&self) {
    //                     dbg!("bye");
    //                 }
    //             }
    //         }
    //     }
    //     let cool = Cool {};
    //     cool.whatever();
    // }
    // error[E0592]: duplicate definitions with name `whatever`

    // Likewise we don't want to add methods to classes in JS in ways that depend on code being run, we want all impls to be automatically added to the class at compile time and thus accessible from anywhere.

    // Rules/algorithm for finding class
    // (in both cases a struct being `use`'d means it must be at the top level of a different module)
    // * Top level impls *
    // The struct for an impl defined at the top level, could be defined at the top level, or `use`'d at the top level
    // 1. look for a struct/enum/class with the same name in the current module.
    // 2. lool for uses with the same name, somehow get access to that list of JsStmts, find the struct and update it. Maybe by the list of (unmatched) impls, with the module module path, so wrapping callers can check for a return -> check if module path matches it's own, else return the impls again. Likewise to get lower ones, pass them as an argument??? The other mod could be in a completely different branch that has already been parsed... I think we need to just add them, with the struct name and module path, to a global vec, then do a second pass where we go through each module and update any classes we have impls for.
    //
    // * Impls in functions *
    // The struct for an impl defined in a function, could be in any parent function, `use`'d in any parent function, defined at the top level, or `use`'d at the top level
    // 1. look for struct in current block/function scope stmts
    // 2. look for use in the current scope
    // 4. look in current module for struct or use (doing this before parent scopes because it is probably quicker and more likely to be there)
    // 3. recursively look in the parent scope for the struct or use
    // Maybe get access to scopes in higher levels by returning any unmatched impls?

    // for the current block/list of stmts, store impl items in a Vec along with the class name
    // After
    let class_name = match &*item_impl.self_ty {
        Type::Path(type_path) => type_path.path.segments.first().unwrap().ident.to_string(),
        _ => todo!(),
    };

    if let Some(trait_) = &item_impl.trait_ {
        if trait_.1.segments.len() != 1 {
            todo!()
        }
        global_data.default_trait_impls_class_mapping.push((
            class_name.clone(),
            trait_.1.segments.first().unwrap().ident.to_string(),
        ));
    }

    let mut impl_stmts = Vec::new();
    for impl_item in &item_impl.items {
        match impl_item {
            ImplItem::Const(impl_item_const) => {
                // impl_item_const
                impl_stmts.push(ImplItemTemp {
                    // class_name: impl_item_const.ident.to_string(),
                    class_name: class_name.clone(),
                    module_path: current_module_path.clone(),
                    item_stmt: JsImplItem::ClassStatic(JsLocal {
                        public: false,
                        export: false,
                        type_: LocalType::Static,
                        lhs: LocalName::Single(impl_item_const.ident.to_string()),
                        value: handle_expr(
                            &impl_item_const.expr,
                            global_data,
                            &current_module_path,
                        ),
                    }),
                })
            }
            ImplItem::Fn(item_impl_fn) => {
                let static_ = if let Some(first_input) = item_impl_fn.sig.inputs.first() {
                    match first_input {
                        FnArg::Receiver(receiver) => false,
                        FnArg::Typed(_) => true,
                    }
                } else {
                    true
                };
                // let private = !export;
                let input_names = item_impl_fn
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

                // TODO this approach for bool_and and add_assign is very limited and won't be possible if 2 differnt types need 2 different implementations for the same method name

                let body_stmts = if class_name == "RustBool" && item_impl_fn.sig.ident == "bool_and"
                {
                    Some(vec![JsStmt::Expr(
                        JsExpr::New(
                            vec!["RustBool".to_string()],
                            vec![JsExpr::Raw("this.jsBoolean && other.jsBoolean".to_string())],
                        ),
                        false,
                    )])

                    // fn add_assign(&mut self, other: RustInteger<T>) {
                    //     self.js_number.0 += other.js_number.0;
                    // }
                } else if class_name == "RustInteger" && item_impl_fn.sig.ident == "add_assign" {
                    Some(vec![JsStmt::Raw(
                        "this.jsNumber += other.jsNumber".to_string(),
                    )])
                } else if class_name == "Option" && item_impl_fn.sig.ident == "eq" {
                    Some(vec![JsStmt::Raw(
                        "return new RustBool(this.id === other.id && JSON.stringify(this.data) === JSON.stringify(other.data))"
                            .to_string(),
                    )])
                } else if class_name == "Option" && item_impl_fn.sig.ident == "ne" {
                    Some(vec![JsStmt::Raw(
                        "return new RustBool(this.id !== other.id || this.data.ne(other.data))"
                            .to_string(),
                    )])
                } else if class_name == "RustBool" && item_impl_fn.sig.ident == "eq" {
                    Some(vec![JsStmt::Raw(
                        "return new RustBool(this.jsBoolean === other.jsBoolean)".to_string(),
                    )])
                } else if class_name == "RustBool" && item_impl_fn.sig.ident == "ne" {
                    Some(vec![JsStmt::Raw(
                        "return new RustBool(this.jsBoolean !== other.jsBoolean)".to_string(),
                    )])
                } else if class_name == "RustString" && item_impl_fn.sig.ident == "clone" {
                    Some(vec![JsStmt::Raw(
                        "return new RustString(this.jsString)".to_string(),
                    )])
                } else {
                    let n_stmts = item_impl_fn.block.stmts.len();
                    let body_stmts = item_impl_fn
                        .block
                        .stmts
                        .clone()
                        .into_iter()
                        .map(|stmt| stmt)
                        .collect::<Vec<_>>();
                    let body_stmts =
                        parse_fn_body_stmts(&body_stmts, global_data, current_module_path);
                    Some(body_stmts)
                };
                if let Some(body_stmts) = body_stmts {
                    impl_stmts.push(
                        // item_impl_fn.sig.ident.to_string(),
                        ImplItemTemp {
                            class_name: class_name.clone(),
                            module_path: current_module_path.clone(),
                            item_stmt: JsImplItem::ClassMethod(
                                class_name.clone(),
                                false,
                                static_,
                                JsFn {
                                    iife: false,
                                    public: false,
                                    export: false,
                                    is_method: true,
                                    async_: item_impl_fn.sig.asyncness.is_some(),
                                    name: camel(item_impl_fn.sig.ident.clone()),
                                    input_names,
                                    body_stmts,
                                },
                            ),
                        },
                    );
                }
            }
            ImplItem::Type(_) => todo!(),
            ImplItem::Macro(_) => todo!(),
            ImplItem::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }
    // TODO can't remember why I added this differentiation
    // if is_module {
    //     global_data.impl_items.extend(impl_stmts);
    // } else {
    //     js_stmts.extend(
    //         impl_stmts
    //             .into_iter()
    //             .map(|ImplItemTemp { item_stmt, .. }| item_stmt)
    //             .collect::<Vec<_>>(),
    //     );
    // }
    global_data.impl_items.extend(impl_stmts);
}

fn handle_item_struct(
    item_struct: &ItemStruct,
    global_data: &GlobalData,
    current_module_path: &Vec<String>,
) -> JsStmt {
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

    let mut methods = Vec::new();

    let mut name = item_struct.ident.to_string();

    // TODO deriving PartialEq for our Option causes a clash with the proper Option, so just manually add it for now
    // fn eq(&self, other: &Self) -> bool
    if name == "Option" {
        let stmt = JsStmt::Raw("return JSON.stringify(this) === JSON.stringify(other)".to_string());
        methods.push((
            name.clone(),
            false,
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

    if let Some(dup) = global_data
        .duplicates
        .iter()
        .find(|dup| dup.name == name && dup.original_module_path == *current_module_path)
    {
        name = dup
            .namespace
            .iter()
            .map(|seg| camel(seg))
            .collect::<Vec<_>>()
            .join("__");
    }

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
        name,
        tuple_struct,
        inputs,
        static_fields: Vec::new(),
        methods,
    })
}

fn handle_item(
    item: Item,
    is_module: bool,
    global_data: &mut GlobalData,
    current_module_path: &mut Vec<String>,
    js_stmts: &mut Vec<JsStmt>,
    current_file_path: &mut Option<PathBuf>,
) {
    match item {
        Item::Const(item_const) => {
            let mut name = item_const.ident.to_string();
            if let Some(dup) = global_data
                .duplicates
                .iter()
                .find(|dup| dup.name == name && dup.original_module_path == *current_module_path)
            {
                name = dup
                    .namespace
                    .iter()
                    .map(|seg| camel(seg))
                    .collect::<Vec<_>>()
                    .join("__");
            }
            let local_stmt = JsStmt::Local(JsLocal {
                export: false,
                public: match item_const.vis {
                    Visibility::Public(_) => true,
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => false,
                },
                type_: LocalType::Var,
                lhs: LocalName::Single(name),
                value: handle_expr(&*item_const.expr, global_data, current_module_path),
            });
            js_stmts.push(local_stmt);
        }
        Item::Enum(item_enum) => {
            js_stmts.push(handle_item_enum(
                item_enum,
                global_data,
                current_module_path,
            ));
        }
        Item::ExternCrate(_) => todo!(),
        Item::Fn(item_fn) => {
            js_stmts.push(handle_item_fn(&item_fn, global_data, current_module_path));
        }
        Item::ForeignMod(_) => todo!(),
        Item::Impl(item_impl) => handle_item_impl(&item_impl, global_data, current_module_path),
        Item::Macro(_) => todo!(),
        Item::Mod(item_mod) => {
            // Notes
            // The `self` keyword is only allowed as the first segment of a path
            // The `crate` keyword is only allowed as the first segment of a path
            // The `super` keyword is only allowed as *one* of the first segments of a path, before any named modules
            // The `super` keyword can be used in multiple segments of a path
            // self might not be that important but crate is and has similar requirements
            // modules *cannot* access anything in their parent scope without explicitly using crate or super, therefore nesting the modules in JS is of no benefit
            // Also need to consider how to use the same Rust module/JS function in multiple places - even though modules are just items and therefore immutable, we still can't have the duplication of code because this could be huge in certain cases. So all modules, both crate modules and sub modules, need to be defined at the top level - no they just need to be accessible from the top level using crate and super, nesting modules doesn't mean duplication because they will always be access at that path anyway.
            // We *could* use a solution requiring replacing self:: paths with absolute paths since self:: *always refers to a module path and self in a method always uses self. since self is an instance not a type/path

            let current_module_name = current_module_path.last().unwrap().clone();
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
            let mut js_stmt_submodule = JsModule {
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
            let stmts = js_stmts_from_syn_items(
                items,
                true,
                current_module_path,
                global_data,
                current_file_path,
            );
            let js_stmt_module = global_data
                .transpiled_modules
                .iter_mut()
                .find(|tm| tm.module_path == current_module_path_copy)
                .unwrap();
            js_stmt_module.stmts = stmts;
            current_module_path.pop();

            // TODO shouldn't be using .export field as this is for importing from separate files. We don't want to add "export " to public values in a module, simply add them to the return statement of the function.
        }
        Item::Static(_) => todo!(),
        Item::Struct(item_struct) => {
            let js_stmt = handle_item_struct(&item_struct, global_data, current_module_path);
            js_stmts.push(js_stmt);
        }
        Item::Trait(item_trait) => {
            handle_item_trait(&item_trait, global_data, current_module_path);
            js_stmts.push(JsStmt::Expr(JsExpr::Vanish, false));
        }
        Item::TraitAlias(_) => todo!(),
        Item::Type(_) => todo!(),
        Item::Union(_) => todo!(),
        Item::Use(_) => {}
        Item::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}

fn handle_item_trait(
    item_trait: &ItemTrait,
    global_data: &mut GlobalData,
    current_module_path: &Vec<String>,
) {
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
                            .map(|stmt| handle_stmt(stmt, global_data, current_module_path))
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

// TODO remove this as it is unnecessary redirection
/// Converts a Vec<syn::Item> to Vec<JsStmt> and moves method impls into their class
///
/// all users (eg crate, fn, file) want to group classes, but only crates want to populate boilerplate
fn js_stmts_from_syn_items(
    items: Vec<Item>,
    // Need to know whether to return a module Object or just a vec of stmts
    is_module: bool,
    // Need to keep of which module we are currently in, for constructing the boilerplate
    current_module: &mut Vec<String>,
    global_data: &mut GlobalData,
    current_file_path: &mut Option<PathBuf>,
) -> Vec<JsStmt> {
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

    for item in items {
        handle_item(
            item,
            is_module,
            global_data,
            current_module,
            &mut js_stmts,
            current_file_path,
        );
    }

    js_stmts
}

#[derive(Default, Clone, Debug)]
struct RustPreludeTypes {
    vec: bool,
    hash_map: bool,
    // TODO need to check which fns return an Option, and set `option` for those
    option: bool,
    some: bool,
    // TODO Is this just null?
    none: bool,
    result: bool,
    ok: bool,
    err: bool,
    assert_eq: bool,
    assert_ne: bool,
    dbg: bool,
    // println: bool,
    // print: bool,
    integer: bool,
    float: bool,
    string: bool,
    bool: bool,
    equals: bool,
}

#[derive(Debug, Clone)]
enum JsImplItem {
    /// This means that `foo() {}` will be used in place of `function foo() {}`  
    ///
    /// Some means it is a method, the first bool is whether it is private and thus should have # prepended to the name, the second bool is whether it is static  
    ///
    /// (class name, private, static, JsFn)  
    ClassMethod(String, bool, bool, JsFn),
    ClassStatic(JsLocal),
}
#[derive(Debug, Clone)]
struct ImplItemTemp {
    /// snake case
    class_name: String,
    /// snake case
    module_path: Vec<String>,
    item_stmt: JsImplItem,
}

#[derive(Debug, Clone)]
struct ModuleData {
    name: String,
    parent_name: Option<String>,
    path: Vec<String>,
    pub_definitions: Vec<String>,
    private_definitions: Vec<String>,
    pub_submodules: Vec<String>,
    private_submodules: Vec<String>,
    /// (snake case item name, snake case use path)
    pub_use_mappings: Vec<(String, Vec<String>)>,
    private_use_mappings: Vec<(String, Vec<String>)>,
    /// Same format as use mapping but has absolute module path
    /// (snake case item name, snake case absolute module path)
    resolved_mappings: Vec<(String, Vec<String>)>,
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
    fn_info: Vec<(String, Vec<String>)>,
}
impl ModuleData {
    fn item_defined_in_module(&self, use_private: bool, item: &String) -> bool {
        let mut definitions = self.pub_definitions.iter();
        if use_private {
            definitions
                .chain(self.private_definitions.iter())
                .any(|definition| definition == item)
        } else {
            definitions.any(|definition| definition == item)
        }
    }
    fn path_starts_with_sub_module(&self, use_private: bool, item: &String) -> bool {
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

#[derive(Debug, Clone)]
struct Duplicate {
    namespace: Vec<String>,
    module_path: Vec<String>,
    name: String,
    original_module_path: Vec<String>,
}

#[derive(Debug, Clone)]
struct ScopedVar {
    name: String,
    mut_: bool,
    mut_ref: bool,
    // TODO
    type_: String,
}

#[derive(Debug, Clone)]
struct GlobalData {
    snippet: bool,
    crate_path: Option<PathBuf>,
    modules: Vec<ModuleData>,
    // TODO doesn't handle capturing scopes which needs rules to mimic how a closure decides to take &, &mut, or ownership
    // NOTE use separate Vecs for vars and fns because not all scopes (for vars) eg blocks are fns
    // NOTE don't want to pop fn after we finish parsing it because it will be called later in the same scope in which it was defined (but also might be called inside itself - recursively), so only want to pop it once it's parent scope completes, so may as well share scoping with vars
    /// (variable, fns)
    scopes: Vec<(Vec<ScopedVar>, Vec<ItemFn>)>,
    // TODO handle closures - which don't have explicitly specified return type, need to infer it from return value
    // scoped_fns: Vec<ItemFn>,
    rust_prelude_types: RustPreludeTypes,
    /// (trait name, impl item)
    default_trait_impls: Vec<(String, JsImplItem)>,
    /// (class name, trait name)
    default_trait_impls_class_mapping: Vec<(String, String)>,
    impl_items: Vec<ImplItemTemp>,
    duplicates: Vec<Duplicate>,
    transpiled_modules: Vec<JsModule>,
}
impl GlobalData {
    fn new(snippet: bool, crate_path: Option<PathBuf>, duplicates: Vec<Duplicate>) -> GlobalData {
        GlobalData {
            snippet,
            crate_path,
            modules: Vec::new(),
            // init with an empty scope to ensure `scopes.last()` always returns something TODO improve this
            scopes: vec![(Vec::new(), Vec::new())],
            // scoped_fns: vec![],
            rust_prelude_types: RustPreludeTypes::default(),
            default_trait_impls_class_mapping: Vec::new(),
            default_trait_impls: Vec::new(),
            impl_items: Vec::new(),
            duplicates,
            transpiled_modules: Vec::new(),
        }
    }
}

/// Match impl items to the classes in a `JsStmtModule`'s stmts and update the classes, recursively doing the same thing for any sub modules
fn update_classes(
    js_stmt_modules: &mut Vec<JsModule>,
    impl_items: &Vec<ImplItemTemp>,
    default_trait_impls_class_mapping: &Vec<(String, String)>,
    default_trait_impls: &Vec<(String, JsImplItem)>,
) {
    for js_stmt_module in js_stmt_modules {
        for stmt in js_stmt_module.stmts.iter_mut() {
            match stmt {
                JsStmt::Class(js_class) => {
                    // add impl methods to class
                    for impl_item in impl_items.clone() {
                        // TODO impl could be in another module?
                        if impl_item.module_path == js_stmt_module.module_path
                            && js_class.name == impl_item.class_name
                        {
                            match impl_item.item_stmt {
                                JsImplItem::ClassStatic(js_local) => {
                                    js_class.static_fields.push(js_local);
                                }
                                JsImplItem::ClassMethod(name, private, static_, js_fn) => {
                                    js_class.methods.push((name, private, static_, js_fn));
                                }
                                stmt => {
                                    dbg!(stmt);
                                    panic!("this JsStmt cannot be an impl item")
                                }
                            }
                        }
                    }

                    // TODO when adding a default impl to a class, we need to know which module/scope it came from in case there are two trait with the same name
                    // TODO also need to only add if there is not an impl which overrides the default
                    // add trait methods with default impl to class
                    for trait_name in default_trait_impls_class_mapping.iter().filter_map(
                        |(class_name, trait_name)| {
                            (class_name == &js_class.name).then_some(trait_name)
                        },
                    ) {
                        for js_impl_item in
                            default_trait_impls
                                .iter()
                                .filter_map(|(trait_name2, js_impl_item)| {
                                    (trait_name == trait_name2).then_some(js_impl_item.clone())
                                })
                        {
                            match js_impl_item {
                                JsImplItem::ClassStatic(js_local) => {
                                    js_class.static_fields.push(js_local);
                                }
                                JsImplItem::ClassMethod(name, private, static_, js_fn) => {
                                    js_class.methods.push((name, private, static_, js_fn));
                                }
                                stmt => {
                                    dbg!(stmt);
                                    panic!("this JsStmt cannot be an impl item")
                                }
                            }
                        }
                    }
                }
                // JsStmt::Module(js_stmt_module) => update_classes(js_stmt_module, impl_items.clone()),
                _ => {}
            }
        }
    }
}

// Similarly to `update_classes`, we need to do a pass to replace all use of top level items like `myFunc()`, `new SomeClass()`, `SomeClass.associatedFunc()` with `this.myFunc()`, `new this.SomeClass()`, `this.SomeClass.associatedFunc()`. This means first getting the names of the top level items, and then not just iterating through the module's statements but though every expression at a top level item could be called absolutely anywhere, eg in an `if` condition.
// What about where we are inside a class method so this refers to the class, not the module?
// Solutions:
// add a unique name like `this.moduleThis = this;` - not possible because it still exists on the `this` which class methods don't have access to.
// add `this.moduleThis` to class objects in init function??
// replace `myFunc()` with `this.thisModule.myFunc();` and add below to init script:
// this.colorModule.greenModule.greenClass.prototype.thisModule = this.colorModule.greenModule;`

// Having "crate" in the module path is useful for representing that the top level module is indeed a module, and for giving it a name that can be looked up in the list. However, it is annoying for eg using the path to create a filepath from

// TODO `names` is probably redundant as we have the same data in `modules`
// Update names
// Determine which names are not globally unique and need namespacing
// TODO should cache the syn::File parsed for each module to avoid repeating this expensive operation (read file and parse) during the parse stage
/// gets names of module level items, creates `ModuleData` for each module, and adds `use` data to module's `.use_mapping`
fn extract_data(
    items: &Vec<Item>,
    // None if we are extracted data for a single file or snippet, rather than an actual crate (so no `mod foo` allowed)
    crate_path: Option<&PathBuf>,
    module_path: &mut Vec<String>,
    // (module path, name)
    names: &mut Vec<(Vec<String>, String)>,
    modules: &mut Vec<ModuleData>,
) {
    // let mut module_path_with_crate = vec!["crate".to_string()];
    // module_path_with_crate.extend(module_path.clone());
    // let current_module_data = modules
    //     .iter_mut()
    //     .find(|module| module.path == *module_path)
    //     .unwrap();
    // let defined_names = &mut current_module_data.defined_names;
    for item in items {
        match item {
            Item::Const(item_const) => {
                names.push((module_path.clone(), item_const.ident.to_string()));

                let current_module_data = modules
                    .iter_mut()
                    .find(|module| module.path == *module_path)
                    .unwrap();
                match item_const.vis {
                    Visibility::Public(_) => current_module_data
                        .pub_definitions
                        .push(item_const.ident.to_string()),
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => current_module_data
                        .private_definitions
                        .push(item_const.ident.to_string()),
                }
            }
            Item::Enum(item_enum) => {
                names.push((module_path.clone(), item_enum.ident.to_string()));

                let current_module_data = modules
                    .iter_mut()
                    .find(|module| module.path == *module_path)
                    .unwrap();
                match item_enum.vis {
                    Visibility::Public(_) => current_module_data
                        .pub_definitions
                        .push(item_enum.ident.to_string()),
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => current_module_data
                        .private_definitions
                        .push(item_enum.ident.to_string()),
                }
            }
            Item::ExternCrate(_) => todo!(),
            Item::Fn(item_fn) => {
                names.push((module_path.clone(), item_fn.sig.ident.to_string()));

                let current_module_data = modules
                    .iter_mut()
                    .find(|module| module.path == *module_path)
                    .unwrap();
                match item_fn.vis {
                    Visibility::Public(_) => current_module_data
                        .pub_definitions
                        .push(item_fn.sig.ident.to_string()),
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => current_module_data
                        .private_definitions
                        .push(item_fn.sig.ident.to_string()),
                }
            }
            Item::ForeignMod(_) => todo!(),
            Item::Impl(_) => {}
            Item::Macro(_) => {}
            Item::Mod(item_mod) => {
                let current_module_data = modules
                    .iter_mut()
                    .find(|module| module.path == *module_path)
                    .unwrap();
                match item_mod.vis {
                    Visibility::Public(_) => current_module_data
                        .pub_submodules
                        .push(item_mod.ident.to_string()),
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => current_module_data
                        .private_submodules
                        .push(item_mod.ident.to_string()),
                }

                let parent_name = module_path.last().map(|x| x.clone());
                module_path.push(item_mod.ident.to_string());

                modules.push(ModuleData {
                    name: item_mod.ident.to_string(),
                    parent_name,
                    path: module_path.clone(),
                    pub_definitions: Vec::new(),
                    private_definitions: Vec::new(),
                    pub_submodules: Vec::new(),
                    private_submodules: Vec::new(),
                    pub_use_mappings: Vec::new(),
                    private_use_mappings: Vec::new(),
                    resolved_mappings: Vec::new(),
                    fn_info: Vec::new(),
                });

                if let Some(content) = &item_mod.content {
                    // TODO how does `mod bar { mod foo; }` work?
                    extract_data(&content.1, crate_path, module_path, names, modules);
                } else {
                    if let Some(crate_path) = crate_path {
                        let mut file_path = crate_path.clone();
                        file_path.push("src");
                        if *module_path == ["crate"] {
                            file_path.push("main.rs");
                        } else {
                            let mut module_path_copy = module_path.clone();
                            // remove "crate"
                            module_path_copy.remove(0);
                            let last = module_path_copy.last_mut().unwrap();
                            last.push_str(".rs");
                            file_path.extend(module_path_copy);
                        }

                        let code = fs::read_to_string(&file_path).unwrap();
                        let file = syn::parse_file(&code).unwrap();
                        extract_data(&file.items, Some(crate_path), module_path, names, modules);
                    } else {
                        panic!("`mod foo` is not allowed in files/modules/snippets, only crates")
                    }
                }
                module_path.pop();
            }
            Item::Static(_) => todo!(),
            Item::Struct(item_struct) => {
                names.push((module_path.clone(), item_struct.ident.to_string()));

                let current_module_data = modules
                    .iter_mut()
                    .find(|module| module.path == *module_path)
                    .unwrap();
                match item_struct.vis {
                    Visibility::Public(_) => current_module_data
                        .pub_definitions
                        .push(item_struct.ident.to_string()),
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => current_module_data
                        .private_definitions
                        .push(item_struct.ident.to_string()),
                }
            }
            Item::Trait(_) => {}
            Item::TraitAlias(_) => todo!(),
            Item::Type(_) => todo!(),
            Item::Union(_) => todo!(),
            Item::Use(item_use) => {
                handle_item_use(&item_use, module_path.clone(), true, modules);
            }
            Item::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }
}

fn update_dup_names(duplicates: &mut Vec<Duplicate>) {
    let dups_copy = duplicates.clone();
    for dup in duplicates.iter_mut() {
        if dups_copy
            .iter()
            .filter(|dup_copy| dup.name == dup_copy.name && dup.namespace == dup_copy.namespace)
            .collect::<Vec<_>>()
            .len()
            > 1
        {
            if dup.module_path != vec!["crate"] {
                dup.namespace.insert(0, dup.module_path.pop().unwrap())
            }
        }
    }
}

fn push_rust_types(global_data: &GlobalData, mut js_stmts: Vec<JsStmt>) -> Vec<JsStmt> {
    let code = include_str!("rust_prelude/option.rs");
    let modules = from_file(code, false);
    assert_eq!(modules.len(), 1);
    let option_module = &modules[0];

    let code = include_str!("rust_prelude/number.rs");
    let modules = from_file(code, false);
    assert_eq!(modules.len(), 1);
    let number_module = &modules[0];

    let code = include_str!("rust_prelude/string.rs");
    let modules = from_file(code, false);
    assert_eq!(modules.len(), 1);
    let string_module = &modules[0];

    let code = include_str!("rust_prelude/bool.rs");
    let modules = from_file(code, false);
    assert_eq!(modules.len(), 1);
    let bool_module = &modules[0];

    // We want to insert prelude stmts at the beginning of js_stmts, but if we do that per item we will reverse the order they appear in the source files. Instead we push them to `prelude_stmts` and then insert that in one go
    let mut prelude_stmts = Vec::new();

    let rust_prelude_types = &global_data.rust_prelude_types;

    let bool_in_other_prelude =
        rust_prelude_types.string || rust_prelude_types.integer || rust_prelude_types.float;

    if rust_prelude_types.vec {
        let mut methods = Vec::new();
        methods.push((
            "new".to_string(),
            false,
            true,
            JsFn {
                iife: false,
                export: false,
                public: false,
                async_: false,
                is_method: true,
                name: "new".to_string(),
                input_names: Vec::new(),
                body_stmts: vec![JsStmt::Raw("this.vec = [];".to_string())],
            },
        ));
        methods.push((
            "push".to_string(),
            false,
            false,
            JsFn {
                iife: false,
                export: false,
                public: false,
                async_: false,
                is_method: true,
                name: "push".to_string(),
                input_names: vec!["elem".to_string()],
                body_stmts: vec![JsStmt::Raw("this.vec.push(elem);".to_string())],
            },
        ));
        prelude_stmts.push(JsStmt::Class(JsClass {
            export: false,
            public: false,
            name: "Vec".to_string(),
            tuple_struct: false,
            inputs: Vec::new(),
            static_fields: Vec::new(),
            methods,
        }));
    }

    if rust_prelude_types.option {
        for stmt in &option_module.stmts {
            match stmt {
                JsStmt::Class(js_class) => {
                    if js_class.name == "Option" {
                        prelude_stmts.push(stmt.clone());
                    }
                }
                JsStmt::ClassMethod(_, _, _, _) => todo!(),
                JsStmt::ClassStatic(_) => todo!(),
                // JsStmt::Local(js_local) => match &js_local.lhs {
                //     LocalName::Single(name) => {
                //         if name == "Some" || name == "None" {
                //             js_stmts.insert(0, stmt.clone());
                //         }
                //     }
                //     LocalName::DestructureObject(_) => todo!(),
                //     LocalName::DestructureArray(_) => todo!(),
                // },
                JsStmt::Local(_) => todo!(),
                JsStmt::Expr(_, _) => todo!(),
                JsStmt::Import(_, _, _) => todo!(),
                JsStmt::Function(_) => todo!(),
                JsStmt::ScopeBlock(_) => todo!(),
                JsStmt::TryBlock(_) => todo!(),
                JsStmt::CatchBlock(_, _) => todo!(),
                JsStmt::Raw(_) => todo!(),
                JsStmt::Comment(_) => todo!(),
            }
        }
    }
    if rust_prelude_types.some {
        prelude_stmts.push(JsStmt::Raw("var Some = Option.Some;".to_string()))
    }
    if rust_prelude_types.none {
        prelude_stmts.push(JsStmt::Raw("var None = Option.None;".to_string()))
    }

    if rust_prelude_types.integer || rust_prelude_types.float {
        for stmt in &number_module.stmts {
            match stmt {
                JsStmt::Class(js_class) => {
                    if js_class.name == "RustInteger" {
                        if rust_prelude_types.integer {
                            prelude_stmts.push(stmt.clone());
                        }
                    } else if js_class.name == "RustFloat" {
                        if rust_prelude_types.float {
                            prelude_stmts.push(stmt.clone());
                        }
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        }
    }

    if rust_prelude_types.string {
        for stmt in &string_module.stmts {
            match stmt {
                JsStmt::Class(js_class) => {
                    // TODO don't need this if check?
                    if js_class.name == "RustString" {
                        prelude_stmts.push(stmt.clone());
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        }
    }

    if rust_prelude_types.bool || bool_in_other_prelude {
        for stmt in &bool_module.stmts {
            match stmt {
                JsStmt::Class(js_class) => {
                    if js_class.name == "RustBool" {
                        prelude_stmts.push(stmt.clone());
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        }
    }

    if rust_prelude_types.equals {
        // TODO write the function in Rust and transpile it
        prelude_stmts.push(JsStmt::Raw(
            "function eq(lhs, rhs) { return JSON.stringify(lhs) === JSON.stringify(rhs); }"
                .to_string(),
        ))
    }
    prelude_stmts.append(&mut js_stmts);
    prelude_stmts
}

pub fn process_items(
    items: Vec<Item>,
    // TODO combine use of all 3 PathBuf into 1
    get_names_crate_path: Option<&PathBuf>,
    global_data_crate_path: Option<PathBuf>,
    entrypoint_path: &mut Option<PathBuf>,
    with_rust_types: bool,
) -> Vec<JsModule> {
    let mut names = Vec::new();
    let mut modules = Vec::new();
    modules.push(ModuleData {
        name: "crate".to_string(),
        parent_name: None,
        path: vec!["crate".to_string()],
        pub_definitions: Vec::new(),
        private_definitions: Vec::new(),
        pub_submodules: Vec::new(),
        private_submodules: Vec::new(),
        pub_use_mappings: Vec::new(),
        private_use_mappings: Vec::new(),
        resolved_mappings: Vec::new(),
        fn_info: Vec::new(),
    });
    let mut get_names_module_path = vec!["crate".to_string()];
    // let mut get_names_crate_path = crate_path.join("src/main.rs");
    extract_data(
        &items,
        get_names_crate_path,
        &mut get_names_module_path,
        &mut names,
        &mut modules,
    );

    // find duplicates
    // TODO account for local functions which shadow these names
    // (name space, module path (which gets popped), name, original module path)

    let mut duplicates = Vec::new();
    for name in &names {
        if names
            .iter()
            .filter(|(module_path, name2)| &name.1 == name2)
            .collect::<Vec<_>>()
            .len()
            > 1
        {
            duplicates.push(Duplicate {
                namespace: Vec::<String>::new(),
                module_path: name.0.clone(),
                name: name.1.clone(),
                original_module_path: name.0.clone(),
            });
        }
    }
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);

    for dup in duplicates.iter_mut() {
        dup.namespace.push(dup.name.clone());
    }

    // resolve_use_stmts(&mut modules);

    let mut global_data = GlobalData::new(false, global_data_crate_path, duplicates.clone());
    global_data.modules = modules;

    global_data.transpiled_modules.push(JsModule {
        public: true,
        name: "crate".to_string(),
        module_path: vec!["crate".to_string()],
        stmts: Vec::new(),
    });
    let mut stmts = js_stmts_from_syn_items(
        items,
        true,
        &mut vec!["crate".to_string()],
        &mut global_data,
        entrypoint_path,
    );

    let stmts = if with_rust_types {
        push_rust_types(&global_data, stmts)
    } else {
        stmts
    };

    let crate_module = global_data
        .transpiled_modules
        .iter_mut()
        .find(|tm| tm.module_path == vec!["crate".to_string()])
        .unwrap();
    crate_module.stmts = stmts;

    update_classes(
        &mut global_data.transpiled_modules,
        &global_data.impl_items,
        &global_data.default_trait_impls_class_mapping,
        &global_data.default_trait_impls,
    );
    // dbg!(&global_data.transpiled_modules);

    // resolve paths to get canonical path to item
    // Update paths to use namespaced names and account for `use` statements
    // We want to find the module where the item is actually defined, not where it is mod, use, pub from other modules
    // Solution:
    // x First find all the items defined in each module
    // Then for each module, add maps of the names of the items that are mod/use'd to the use path
    // go through each module and reconcile each item with the use mapping, and that with the module path where the item is actually defined

    // Remember that use might only be `use`ing a module, and then completing the path to the actual item in the code. So the final step of reconciliation will always need make use of the actual paths/items in the code
    // dbg!(global_data.modules);

    // and module name comments when there is more than 1 module
    if global_data.transpiled_modules.len() > 1 {
        for module in &mut global_data.transpiled_modules {
            module.stmts.insert(
                0,
                JsStmt::Comment(if module.module_path == ["crate"] {
                    "crate".to_string()
                } else {
                    module
                        .module_path
                        .iter()
                        .cloned()
                        .skip(1)
                        .collect::<Vec<_>>()
                        .join("::")
                }),
            );
        }
    }

    global_data.transpiled_modules.clone()
}

pub fn modules_to_string(modules: &Vec<JsModule>, lib: bool) -> String {
    if lib {
        todo!()
    } else {
        modules
            .iter()
            .map(|module| module.js_string())
            .collect::<Vec<_>>()
            .join("\n\n")
    }
}

pub fn from_crate(crate_path: PathBuf, with_rust_types: bool, lib: bool) -> String {
    // dbg!(&main_path);
    // TODO

    let code = fs::read_to_string(crate_path.join("src").join("main.rs")).unwrap();
    let mut entrypoint_path = Some(crate_path.join("src").join("main.rs"));
    let crate_path_copy = crate_path.clone();
    let get_names_crate_path = Some(&crate_path_copy);
    let global_data_crate_path = Some(crate_path.clone());
    let file = syn::parse_file(&code).unwrap();
    let items = file.items;
    // let mut current_file_path = vec!["main.rs".to_string()];

    let modules = process_items(
        items,
        get_names_crate_path,
        global_data_crate_path,
        &mut entrypoint_path,
        with_rust_types,
    );
    modules_to_string(&modules, lib)
}

// Given every file *is* a module, and we concatenate all modules, including inline ones, into a single file, we should treat transpiling individual files *or* module blocks the same way
// Modules defined within a scope, eg a block, are not global and only accessible from that scope, but are treated the same way as other modules in that they are made global to the scope in which they are defined
pub fn from_file(code: &str, with_rust_types: bool) -> Vec<JsModule> {
    let mut entrypoint_path = None;
    let get_names_crate_path = None;
    let global_data_crate_path = None;
    let file = syn::parse_file(code).unwrap();
    let items = file.items;

    process_items(
        items,
        get_names_crate_path,
        global_data_crate_path,
        &mut entrypoint_path,
        with_rust_types,
    )
}

pub fn from_block(code: &str, with_rust_types: bool) -> Vec<JsStmt> {
    // let file = syn::parse_file(code).unwrap();
    let expr_block = syn::parse_str::<ExprBlock>(code).unwrap();

    // let mut names = Vec::new();
    let mut modules = Vec::new();
    modules.push(ModuleData {
        name: "crate".to_string(),
        parent_name: None,
        path: vec!["crate".to_string()],
        pub_definitions: Vec::new(),
        private_definitions: Vec::new(),
        pub_submodules: Vec::new(),
        private_submodules: Vec::new(),
        pub_use_mappings: Vec::new(),
        private_use_mappings: Vec::new(),
        resolved_mappings: Vec::new(),
        fn_info: Vec::new(),
    });
    let mut get_names_module_path = vec!["crate".to_string()];

    // let mut get_names_crate_path = crate_path.join("src/main.rs");
    // let mut get_names_crate_path = crate_path.clone();
    // extract_data(
    //     &file.items,
    //     None,
    //     &mut get_names_module_path,
    //     &mut names,
    //     &mut modules,
    // );

    // let mut duplicates = Vec::new();
    // for name in &names {
    //     if names
    //         .iter()
    //         .filter(|(module_path, name2)| &name.1 == name2)
    //         .collect::<Vec<_>>()
    //         .len()
    //         > 1
    //     {
    //         duplicates.push(Duplicate {
    //             namespace: Vec::<String>::new(),
    //             module_path: name.0.clone(),
    //             name: name.1.clone(),
    //             original_module_path: name.0.clone(),
    //         });
    //     }
    // }
    // update_dup_names(&mut duplicates);
    // update_dup_names(&mut duplicates);
    // update_dup_names(&mut duplicates);
    // update_dup_names(&mut duplicates);
    // update_dup_names(&mut duplicates);
    // update_dup_names(&mut duplicates);

    // for dup in duplicates.iter_mut() {
    //     dup.namespace.push(dup.name.clone());
    // }

    // resolve_use_stmts(&mut modules);

    let mut global_data = GlobalData::new(false, None, Vec::new());
    global_data.modules = modules;

    global_data.transpiled_modules.push(JsModule {
        public: true,
        name: "crate".to_string(),
        module_path: vec!["crate".to_string()],
        stmts: Vec::new(),
    });
    // let stmts = js_stmts_from_syn_items(
    //     file.items,
    //     true,
    //     &mut vec!["crate".to_string()],
    //     &mut global_data,
    //     &mut None,
    // );
    let mut stmts = expr_block
        .block
        .stmts
        .iter()
        .map(|stmt| handle_stmt(stmt, &mut global_data, &vec!["crate".to_string()]))
        .collect::<Vec<_>>();

    let stmts = if with_rust_types {
        push_rust_types(&global_data, stmts)
    } else {
        stmts
    };

    let crate_module = global_data
        .transpiled_modules
        .iter_mut()
        .find(|tm| tm.module_path == vec!["crate".to_string()])
        .unwrap();
    crate_module.stmts = stmts;

    update_classes(
        &mut global_data.transpiled_modules,
        &global_data.impl_items,
        &global_data.default_trait_impls_class_mapping,
        &global_data.default_trait_impls,
    );

    // and module name comments when there is more than 1 module
    if global_data.transpiled_modules.len() > 1 {
        for module in &mut global_data.transpiled_modules {
            // dbg!(&module);
            module.stmts.insert(
                0,
                JsStmt::Comment(if module.module_path == ["crate"] {
                    "crate".to_string()
                } else {
                    module
                        .module_path
                        .iter()
                        .cloned()
                        .skip(1)
                        .collect::<Vec<_>>()
                        .join("::")
                }),
            );
        }
    }

    global_data.transpiled_modules[0].stmts.clone()
}

// TODO combine this with from_file
pub fn from_module(code: &str, with_vec: bool) -> Vec<JsStmt> {
    let item_mod = syn::parse_str::<ItemMod>(code).unwrap();
    let items = item_mod.content.unwrap().1;
    let mut current_module = Vec::new();
    let mut global_data = GlobalData::new(false, None, Vec::new());
    js_stmts_from_syn_items(
        items,
        true,
        &mut current_module,
        &mut global_data,
        &mut None,
    )
}

pub fn from_fn(code: &str) -> Vec<JsStmt> {
    let item_fn = syn::parse_str::<ItemFn>(code).unwrap();

    let mut js_stmts = Vec::new();
    for stmt in &item_fn.block.stmts {
        let js_stmt = handle_stmt(
            stmt,
            &mut GlobalData::new(true, None, Vec::new()),
            &Vec::new(),
        );
        js_stmts.push(js_stmt);
    }
    js_stmts
}

// pub fn from_block(code: &str) -> Vec<JsStmt> {
//     let expr_block = syn::parse_str::<ExprBlock>(code).unwrap();

//     let mut js_stmts = Vec::new();
//     for stmt in &expr_block.block.stmts {
//         let js_stmt = handle_stmt(
//             stmt,
//             &mut GlobalData::new(true, None, Vec::new()),
//             &vec!["crate".to_string()],
//         );
//         js_stmts.push(js_stmt);
//     }
//     js_stmts
// }

#[derive(Clone, Debug)]
pub enum JsOp {
    Add,
    Sub,
    AddAssign,
    And,
    Or,
    Eq,
    NotEq,
    Gt,
    GtEq,
    Lt,
    Rem,
}
impl JsOp {
    fn js_string(&self) -> &str {
        match self {
            JsOp::Add => "+",
            JsOp::Sub => "-",
            JsOp::AddAssign => "+=",
            JsOp::And => "&&",
            JsOp::Or => "||",
            JsOp::Eq => "===",
            JsOp::NotEq => "!==",
            JsOp::Gt => ">",
            JsOp::GtEq => ">=",
            JsOp::Lt => "<",
            JsOp::Rem => "%",
        }
    }
    fn from_binop(binop: BinOp) -> JsOp {
        match binop {
            BinOp::Add(_) => JsOp::Add,
            BinOp::Sub(_) => JsOp::Sub,
            BinOp::Mul(_) => todo!(),
            BinOp::Div(_) => todo!(),
            BinOp::Rem(_) => JsOp::Rem,
            BinOp::And(_) => JsOp::And,
            BinOp::Or(_) => JsOp::Or,
            BinOp::BitXor(_) => todo!(),
            BinOp::BitAnd(_) => todo!(),
            BinOp::BitOr(_) => todo!(),
            BinOp::Shl(_) => todo!(),
            BinOp::Shr(_) => todo!(),
            BinOp::Eq(_) => JsOp::Eq,
            BinOp::Lt(_) => JsOp::Lt,
            BinOp::Le(_) => todo!(),
            BinOp::Ne(_) => JsOp::NotEq,
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
        }
    }
}

#[derive(Clone, Debug)]
pub enum JsExpr {
    Array(Vec<JsExpr>),
    /// (async, block, inputs, body)
    ArrowFn(bool, bool, Vec<String>, Vec<JsStmt>),
    Assignment(Box<JsExpr>, Box<JsExpr>),
    Await(Box<JsExpr>),
    Binary(Box<JsExpr>, JsOp, Box<JsExpr>),
    /// Will only make itself disappear
    Blank,
    Block(Vec<JsStmt>),
    Break,
    /// (const/let/var, left, right)
    /// use const for immutatble, let for mutable, var for shadowing
    Declaration(bool, String, Box<JsExpr>),
    /// (base var name, field name)
    Field(Box<JsExpr>, String),
    Fn(JsFn),
    /// (name, args)
    FnCall(Box<JsExpr>, Vec<JsExpr>),
    /// (pat, expr, block)
    ForLoop(String, Box<JsExpr>, Vec<JsStmt>),
    Index(Box<JsExpr>, Box<JsExpr>),
    /// `if else` statements are achieved by nesting an additional if statement as the fail arg.
    /// A problem is that Some assignment triggers a `var = x;`, however we also need to know whether we are doing assignment in nested If's (if else) but without adding a new var declaration. need to add another flag just to say when we need to declare the var
    ///
    If(JsIf),
    LitInt(i32),
    LitFloat(f32),
    LitStr(String),
    LitBool(bool),
    /// (receiver, method name, method args)
    /// TODO assumes receiver is single var
    MethodCall(Box<JsExpr>, String, Vec<JsExpr>),
    Minus(Box<JsExpr>),
    /// (Class path, args)
    New(Vec<String>, Vec<JsExpr>),
    Null,
    Not(Box<JsExpr>),
    Object(Vec<(String, Box<JsExpr>)>),
    ObjectForModule(Vec<JsStmt>),
    Paren(Box<JsExpr>),
    /// like obj::inner::mynumber -> obj.inner.mynumber;
    Path(Vec<String>),
    Raw(String),
    Return(Box<JsExpr>),
    ThrowError(String),
    /// Will make the entire statement disappear no matter where it is nested?
    Vanish,
    Var(String),
    // Class(JsClass),
    While(Box<JsExpr>, Vec<JsStmt>),
}

impl JsExpr {
    fn js_string(&self) -> String {
        match self {
            JsExpr::LitInt(int) => int.to_string(),
            // NOTE `(5.).to_string()` is "5" not "5." or "5.0"
            JsExpr::LitFloat(float) => float.to_string(),
            JsExpr::LitStr(text) => format!(r#""{text}""#),
            JsExpr::LitBool(bool) => bool.to_string(),
            JsExpr::Object(fields) => {
                format!(
                    "{{\n{}\n}}",
                    fields
                        .iter()
                        .map(|(member, expr)| format!("{member}: {}", expr.js_string()))
                        .collect::<Vec<_>>()
                        .join(",\n")
                )
            }
            JsExpr::Vanish => String::new(),
            JsExpr::Blank => String::new(),
            JsExpr::FnCall(func, args) => format!(
                "{}({})",
                func.js_string(),
                args.iter()
                    .map(|arg| arg.js_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            JsExpr::ArrowFn(async_, block, inputs, body) => {
                let sig = if inputs.len() == 1 {
                    inputs.get(0).unwrap().clone()
                } else {
                    format!("({})", inputs.join(", "))
                };
                // TODO single objects returned by concise body must be wrapped in parenthesis
                let body = if *block {
                    body.iter()
                        .enumerate()
                        .map(|(i, stmt)| handle_fn_body_stmt(i, stmt, body.len()))
                        .collect::<Vec<_>>()
                        .join("\n")
                } else {
                    if body.len() > 1 {
                        panic!("closures with no block should only have 1 statement")
                    } else {
                        body.first().unwrap().js_string()
                    }
                };
                format!(
                    "{}{} => {}",
                    if *async_ { "async " } else { "" },
                    sig,
                    if *block {
                        format!("{{\n{}\n}}", body)
                    } else {
                        body
                    }
                )
            }
            JsExpr::Binary(left, op, right) => format!(
                "{} {} {}",
                left.js_string(),
                op.js_string(),
                right.js_string()
            ),
            JsExpr::Var(var_name) => var_name.clone(),
            JsExpr::Field(base, field_name) => format!("{}.{field_name}", base.js_string()),
            JsExpr::New(path, args) => format!(
                "new {}({})",
                path.join("."),
                args.iter()
                    .map(|arg| arg.js_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            JsExpr::MethodCall(receiver_name, method_name, args) => {
                format!(
                    "{}.{}({})",
                    receiver_name.js_string(),
                    method_name,
                    args.iter()
                        .map(|arg| arg.js_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            JsExpr::Path(segments) => segments.join("."),
            JsExpr::Assignment(left, right) => {
                format!("{} = {}", left.js_string(), right.js_string())
            }
            JsExpr::ForLoop(pat, expr, block) => {
                format!(
                    "for (var {pat} of {}) {{\n{}\n}}",
                    expr.js_string(),
                    block
                        .iter()
                        .map(|expr| expr.js_string())
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            JsExpr::Index(expr, index) => {
                format!("{}[{}]", expr.js_string(), index.js_string())
            }
            JsExpr::Await(expr) => format!("await {}", expr.js_string()),
            JsExpr::While(cond, block) => format!(
                "while ({}) {{\n{}\n}}",
                cond.js_string(),
                block
                    .iter()
                    .map(|stmt| stmt.js_string())
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            JsExpr::If(js_if) => js_if.if_expr_to_string(),
            JsExpr::Declaration(_, name, expr) => format!("var {name} = {}", expr.js_string()),
            JsExpr::Break => "break".to_string(),
            JsExpr::Not(expr) => format!("!{}", expr.js_string()),
            JsExpr::Block(stmts) => {
                let stmts = stmts
                    .iter()
                    .map(|stmt| stmt.js_string())
                    .collect::<Vec<_>>()
                    .join("\n");
                format!("{{\n{}\n}}", stmts)
            }
            JsExpr::Minus(expr) => format!("-{}", expr.js_string()),
            JsExpr::Paren(expr) => format!("({})", expr.js_string()),
            JsExpr::Null => "null".to_string(),
            JsExpr::Return(expr) => format!("return {}", expr.js_string()),
            JsExpr::Array(elems) => format!(
                "[{}]",
                elems
                    .iter()
                    .map(|elem| elem.js_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            JsExpr::Fn(js_fn) => js_fn.js_string(),
            JsExpr::ObjectForModule(js_stmts) => {
                let js_stmt_module = JsModule {
                    public: false,
                    name: "whatever".to_string(),
                    module_path: vec![],
                    stmts: js_stmts.clone(),
                };
                js_stmt_module.js_string()
            }
            JsExpr::Raw(text) => text.clone(),
            JsExpr::ThrowError(message) => {
                // TODO improve this - ideally use existing code eg from rustc

                let parts = message
                    .split(",")
                    .into_iter()
                    .map(|part| part.trim().to_string())
                    .collect::<Vec<String>>();
                let expanded_message = if parts.len() > 1 {
                    let mut text = parts[0].clone();
                    if text.is_ascii() {
                        text.remove(0);
                        text.insert(0, '`');
                        text.pop();
                        text.push('`');
                    } else {
                        todo!()
                    }
                    text = text.replacen("{}", format!("${{{}}}", &parts[1]).as_str(), 1);
                    text
                } else if parts[0].chars().next() == Some('"') {
                    parts[0].clone()
                } else {
                    format!(r#""{}""#, parts[0])
                };

                // let lhs = parts.next().unwrap();
                // let lhs = syn::parse_str::<syn::Expr>(lhs).unwrap();
                // let lhs = handle_expr(&lhs, global_data, current_module_path);

                format!(r#"throw new Error({expanded_message})"#)
            }
        }
    }
}

#[derive(Clone, Debug)]
struct JsIf {
    /// The name of the initialised var we are assigning to in the final statement of the block
    assignment: Option<LocalName>,
    /// Whether to prepend the if statement with eg `var result = `
    /// TODO not sure why this is necessary and not handle by the original `let` `Local` statement
    declare_var: bool,
    condition: Box<JsExpr>,
    succeed: Vec<JsStmt>,
    /// syn has an expr as the else branch, rather than an iter of statements - because the expr might be another if expr, not always a block
    fail: Option<Box<JsExpr>>,
}

// TODO Make a struct called If with these fields so I can define js_string() on the struct and not have this fn
// TODO we want to know if the if is a single statment being return from a fn and if so in line the return in the branches rather than use an assignment var
impl JsIf {
    fn if_expr_to_string(&self) -> String {
        let JsIf {
            assignment,
            declare_var,
            condition: cond,
            succeed,
            fail: else_,
        } = self;
        let else_ = if let Some(else_) = else_ {
            match &**else_ {
                // if else {}
                JsExpr::If(js_if) => format!(
                    " else {}",
                    JsExpr::If(JsIf {
                        assignment: assignment.clone(),
                        declare_var: false,
                        condition: js_if.condition.clone(),
                        succeed: js_if.succeed.clone(),
                        fail: js_if.fail.clone()
                    })
                    .js_string()
                ),
                // else {}
                _ => {
                    let thing = match &**else_ {
                        // else { block of stmts }
                        JsExpr::Block(stmts) => stmts
                            .iter()
                            .enumerate()
                            .map(|(i, stmt)| {
                                if i == stmts.len() - 1 {
                                    if let Some(assignment) = assignment {
                                        let is_error = match stmt {
                                            JsStmt::Expr(expr, _) => match expr {
                                                JsExpr::ThrowError(_) => todo!(),
                                                _ => false,
                                            },
                                            _ => false,
                                        };
                                        format!(
                                            "{} = {};",
                                            assignment.js_string(),
                                            stmt.js_string()
                                        )
                                    } else {
                                        stmt.js_string()
                                    }
                                } else {
                                    stmt.js_string()
                                }
                            })
                            .collect::<Vec<_>>()
                            .join("\n"),
                        // else { expr }
                        _ => {
                            if let Some(assignment) = assignment {
                                let is_error = match **else_ {
                                    JsExpr::ThrowError(_) => true,
                                    _ => false,
                                };
                                // let is_error = match stmt {
                                //     JsStmt::Expr(expr, _) => match expr {
                                //         JsExpr::ThrowError(_) => true,
                                //         _ => false,
                                //     },
                                //     _ => false,
                                // };
                                if is_error {
                                    format!("{};", else_.js_string())
                                } else {
                                    // format!("{} = {};", assignment.js_string(), stmt.js_string())
                                    format!("{} = {};", assignment.js_string(), else_.js_string())
                                }
                            } else {
                                else_.js_string()
                            }
                        }
                    };
                    format!(" else {{\n{}\n}}", thing)
                }
            }
        } else {
            "".to_string()
        };
        let assignment_str = if let Some(lhs) = assignment {
            if *declare_var {
                let local = JsLocal {
                    public: false,
                    export: false,
                    type_: LocalType::Var,
                    lhs: lhs.clone(),
                    value: JsExpr::Blank,
                };
                format!("{}\n", local.js_string())
                // "jargallleee".to_string()
            } else {
                "".to_string()
            }
        } else {
            "".to_string()
        };

        format!(
            "{}if ({}) {{\n{}\n}}{}",
            assignment_str,
            cond.js_string(),
            succeed
                .iter()
                .enumerate()
                .map(|(i, stmt)| {
                    if i == succeed.len() - 1 {
                        if let Some(assignment) = assignment {
                            // TODO not sure how to handle `let (var1, var2) = if true { (1, 2) } else { (3, 4) };`. I think we would need to use:
                            // `var temp;`
                            // `if (true) { temp = [1, 2] } else { temp = [3, 4] }`
                            // `var [var1, var2] = temp;`

                            // _ => {
                            //     if *semi {
                            //         format!("{};", js_expr.js_string())
                            //     } else if i == self.body_stmts.len() - 1 {
                            //         format!("return {};", js_expr.js_string())
                            //     } else {
                            //         js_expr.js_string()
                            //     }
                            // }

                            let is_error = match stmt {
                                JsStmt::Expr(expr, _) => match expr {
                                    JsExpr::ThrowError(_) => true,
                                    _ => false,
                                },
                                _ => false,
                            };
                            if is_error {
                                stmt.js_string()
                            } else {
                                format!("{} = {};", assignment.js_string(), stmt.js_string())
                            }
                        } else {
                            stmt.js_string()
                        }
                    } else {
                        stmt.js_string()
                    }
                })
                .collect::<Vec<_>>()
                .join("\n"),
            else_
        )
    }
}

// ::new()/Constructor must assign all fields of class
#[derive(Clone, Debug)]
pub struct JsClass {
    /// None if class is not defined at module level
    // module_path: Option<Vec<String>>,
    public: bool,
    export: bool,
    tuple_struct: bool,
    name: String,
    /// we are assuming input names is equivalent to field names
    inputs: Vec<String>,
    static_fields: Vec<JsLocal>,
    /// (class name, private, static, JsFn)  
    methods: Vec<(String, bool, bool, JsFn)>,
}

#[derive(Clone, Debug)]
pub enum LocalType {
    Var,
    Const,
    Let,
    Static,
}

#[derive(Clone, Debug)]
enum DestructureValue {
    /// A simple destructure like `var { a } = obj;`
    KeyName(String),
    /// A rename destructure like `var { a: b } = obj;`
    Rename(String, String),
    /// A nested destructure like `var { a: { b } } = obj;`
    Nesting(String, DestructureObject),
}
impl DestructureValue {
    fn js_string(&self) -> String {
        match self {
            DestructureValue::KeyName(key) => key.clone(),
            DestructureValue::Rename(key, new_name) => format!("{key}: {new_name}"),
            DestructureValue::Nesting(key, destructure_object) => {
                format!("{key}: {}", destructure_object.js_string())
            }
        }
    }
}

#[derive(Clone, Debug)]
struct DestructureObject(Vec<DestructureValue>);
impl DestructureObject {
    fn js_string(&self) -> String {
        format!(
            "{{ {} }}",
            self.0
                .iter()
                .map(|destructure_value| destructure_value.js_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Clone, Debug)]
pub enum LocalName {
    Single(String),
    DestructureObject(DestructureObject),
    DestructureArray(Vec<LocalName>),
}
impl LocalName {
    fn js_string(&self) -> String {
        match self {
            LocalName::Single(name) => name.clone(),
            LocalName::DestructureObject(destructure_object) => destructure_object.js_string(),
            LocalName::DestructureArray(destructure_array) => format!(
                "[ {} ]",
                destructure_array
                    .iter()
                    .map(|destructure_object| destructure_object.js_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct JsLocal {
    public: bool,
    export: bool,
    type_: LocalType,
    lhs: LocalName,
    value: JsExpr,
}
impl JsLocal {
    fn js_string(&self) -> String {
        let lhs = self.lhs.js_string();
        let var_type = match self.type_ {
            LocalType::Var => "var",
            LocalType::Const => "const",
            LocalType::Let => "let",
            LocalType::Static => "static",
        };

        // check if there is any shadowing in scope and use var instead
        match &self.value {
            // TODO what if we want to declare a null var like `var myvar;` eg prior to if statement
            JsExpr::Vanish => String::new(),
            JsExpr::Blank => format!("{var_type} {lhs};"),
            value_js_expr => format!("{var_type} {lhs} = {};", value_js_expr.js_string()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct JsFn {
    iife: bool,
    public: bool,
    export: bool,
    async_: bool,
    is_method: bool,
    name: String,
    input_names: Vec<String>,
    body_stmts: Vec<JsStmt>,
}

/// Adds return and semi to expr being returned. For an if expression this means we also need to get the name of the assignment var that needs returning  
fn handle_fn_body_stmt(i: usize, stmt: &JsStmt, len: usize) -> String {
    match stmt {
        JsStmt::Local(js_local) => js_local.js_string(),
        JsStmt::Expr(js_expr, semi) => match js_expr {
            JsExpr::If(js_if) => {
                if i == len - 1 {
                    // TODO wrongly assuming that all single if expr bodys should be returned
                    if let Some(assignment) = &js_if.assignment {
                        format!(
                            "{}\nreturn {};",
                            js_expr.js_string(),
                            assignment.js_string()
                        )
                    } else {
                        js_expr.js_string()
                    }
                } else {
                    js_expr.js_string()
                }
            }
            JsExpr::Block(_) => js_expr.js_string(),
            JsExpr::While(_, _) => js_expr.js_string(),
            JsExpr::ForLoop(_, _, _) => js_expr.js_string(),
            JsExpr::Vanish => "".to_string(),
            _ => {
                if *semi {
                    format!("{};", js_expr.js_string())
                } else if i == len - 1 {
                    format!("return {};", js_expr.js_string())
                } else {
                    js_expr.js_string()
                }
            }
        },
        JsStmt::Import(_, _, _) => todo!(),
        JsStmt::Function(_) => stmt.js_string(),
        JsStmt::Class(_) => stmt.js_string(),
        JsStmt::ClassMethod(_, _, _, _) => stmt.js_string(),
        JsStmt::ClassStatic(_) => stmt.js_string(),
        JsStmt::Raw(_) => stmt.js_string(),
        JsStmt::ScopeBlock(_) => stmt.js_string(),
        JsStmt::TryBlock(_) => stmt.js_string(),
        JsStmt::CatchBlock(_, _) => stmt.js_string(),
        JsStmt::Comment(_) => stmt.js_string(),
    }
}
impl JsFn {
    fn js_string(&self) -> String {
        // TODO private fields and methods should be prepended with `#` like `#private_method() {}` but this would require also prepending all callsites of the the field or method, which requires more sophisticated AST analysis than we currently want to do.
        let body_stmts = self
            .body_stmts
            .iter()
            .enumerate()
            .map(|(i, stmt)| handle_fn_body_stmt(i, stmt, self.body_stmts.len()))
            .collect::<Vec<_>>()
            .join("\n");
        let fn_string = format!(
            "{}{}{}{}({}) {{\n{}\n}}",
            if self.export && !self.is_method {
                "export default "
            } else {
                ""
            },
            if self.async_ { "async " } else { "" },
            if self.is_method { "" } else { "function " },
            self.name,
            self.input_names.join(", "),
            body_stmts
        );
        if self.iife {
            format!("({})()", fn_string)
        } else {
            fn_string
        }
    }
}

#[derive(Clone, Debug)]
pub struct JsModule {
    public: bool,
    /// camelCase JS name
    name: String,
    /// snake_case Rust path starting with "crate"
    module_path: Vec<String>,
    // TODO consider having JsItems like syn::Items to enforce what is allowed at the module level
    stmts: Vec<JsStmt>,
}
impl JsModule {
    pub fn js_string(&self) -> String {
        self.stmts
            .iter()
            .map(|stmt| stmt.js_string())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

// pub struct JsImportPath {}
#[derive(Clone, Debug)]
pub enum JsStmt {
    Class(JsClass),
    /// This means that `foo() {}` will be used in place of `function foo() {}`  
    ///
    /// Some means it is a method, the first bool is whether it is private and thus should have # prepended to the name, the second bool is whether it is static  
    ///
    /// (class name, private, static, JsFn)  
    ClassMethod(String, bool, bool, JsFn),
    ClassStatic(JsLocal),
    Local(JsLocal),
    /// (expr, closing semicolon)
    Expr(JsExpr, bool),
    /// (default export name, names of the exports to be imported, module path)
    Import(Option<String>, Vec<String>, Vec<String>),
    Function(JsFn),
    ScopeBlock(Vec<JsStmt>),
    TryBlock(Vec<JsStmt>),
    CatchBlock(String, Vec<JsStmt>),
    Raw(String),
    /// Unlike the other variants this *only* has meaning for the parsing/transpiling, and isn't output (except maybe comments for debugging?)
    ///
    /// (path, item name)
    Comment(String),
}

impl JsStmt {
    /// Need to keep track of which item is public so we know what is item are made available when a * glob is used
    pub fn is_pub(&self) -> bool {
        match self {
            JsStmt::Class(js_class) => js_class.public,
            JsStmt::ClassMethod(_, _, _, _) => todo!(),
            JsStmt::ClassStatic(_) => todo!(),
            JsStmt::Local(js_local) => js_local.public,
            JsStmt::Expr(_, _) => todo!(),
            JsStmt::Import(_, _, _) => todo!(),
            JsStmt::Function(js_fn) => js_fn.public,
            JsStmt::ScopeBlock(_) => todo!(),
            JsStmt::TryBlock(_) => todo!(),
            JsStmt::CatchBlock(_, _) => todo!(),
            JsStmt::Raw(_) => todo!(),
            JsStmt::Comment(_) => todo!(),
        }
    }
    pub fn js_string(&self) -> String {
        match self {
            JsStmt::Local(local) => local.js_string(),
            JsStmt::Expr(expr, closing_semi) => {
                if *closing_semi {
                    format!("{};", expr.js_string())
                } else {
                    expr.js_string()
                }
            }
            JsStmt::Import(default, exports, module) => {
                let module = module
                    .iter()
                    .map(|path_seg| {
                        if [".", ".."].contains(&path_seg.as_str()) {
                            path_seg.clone()
                        } else {
                            path_seg
                                .split(".")
                                .enumerate()
                                .map(|(i, word)| {
                                    if i == 0 && path_seg.split(".").count() > 1 {
                                        AsPascalCase(word).to_string()
                                    } else {
                                        AsKebabCase(word).to_string()
                                    }
                                })
                                .collect::<Vec<_>>()
                                .join(".")
                        }
                    })
                    .collect::<Vec<_>>()
                    .join("/");
                format!(
                    r#"import{}{} from "{}";"#,
                    if let Some(default) = default {
                        format!(" {default}")
                    } else {
                        "".to_string()
                    },
                    if exports.len() > 0 {
                        let exports = exports
                            .iter()
                            .map(|export| {
                                if export.chars().all(|c| c.is_uppercase()) {
                                    camel(export)
                                } else if export.chars().next().unwrap().is_ascii_uppercase() {
                                    AsPascalCase(export).to_string()
                                } else {
                                    camel(export)
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(", ");
                        format!(" {{ {} }}", exports)
                    } else {
                        "".to_string()
                    },
                    module
                )
            }
            JsStmt::Function(js_fn) => js_fn.js_string(),
            JsStmt::Class(js_class) => {
                format!(
                    "class {} {{\n{}{}\n{}}}",
                    js_class.name,
                    if js_class.inputs.len() > 0 {
                        format!(
                            "constructor({}) {{\n{}\n}}\n",
                            js_class.inputs.join(", "),
                            js_class
                                .inputs
                                .iter()
                                .enumerate()
                                .map(|(i, input)| if js_class.tuple_struct {
                                    format!("this[{i}] = {input};")
                                } else {
                                    format!("this.{input} = {input};")
                                })
                                .collect::<Vec<_>>()
                                .join(" "),
                        )
                    } else {
                        "".to_string()
                    },
                    js_class
                        .static_fields
                        .iter()
                        .map(|field| field.js_string())
                        .collect::<Vec<_>>()
                        .join("\n"),
                    js_class
                        .methods
                        .iter()
                        .map(|method| format!(
                            "{}{}",
                            if method.2 { "static " } else { "" },
                            method.3.js_string()
                        ))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            JsStmt::ClassMethod(_, _, _, _) => todo!(),
            JsStmt::ClassStatic(_) => todo!(),
            JsStmt::Raw(raw_js) => raw_js.clone(),
            JsStmt::ScopeBlock(stmts) => {
                format!(
                    "{{\n{}\n}}",
                    stmts
                        .iter()
                        .map(|s| s.js_string())
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            JsStmt::TryBlock(try_block) => {
                format!(
                    "try {{\n{}\n}}",
                    try_block
                        .iter()
                        .map(|s| s.js_string())
                        .collect::<Vec<_>>()
                        .join("\n"),
                )
            }
            JsStmt::CatchBlock(err_var_name, catch_block) => {
                format!(
                    "catch ({}) {{\n{}\n}}",
                    err_var_name,
                    catch_block
                        .iter()
                        .map(|s| s.js_string())
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            JsStmt::Comment(text) => format!("// {text}"),
        }
    }
}

fn parse_fn_body_stmts(
    stmts: &Vec<Stmt>,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> Vec<JsStmt> {
    stmts
        .iter()
        .enumerate()
        .map(|(i, stmt)| {
            // Manually set assignment var name for if expressions that are a return stmt
            if i == stmts.len() - 1 {
                match stmt {
                    Stmt::Expr(expr, semi) => match expr {
                        Expr::If(expr_if) => {
                            if semi.is_some() {
                                handle_stmt(stmt, global_data, current_module)
                            } else {
                                let condition = Box::new(JsExpr::Field(
                                    Box::new(JsExpr::Paren(Box::new(handle_expr(
                                        &*expr_if.cond,
                                        global_data,
                                        current_module,
                                    )))),
                                    "jsBoolean".to_string(),
                                ));
                                JsStmt::Expr(
                                    JsExpr::If(JsIf {
                                        assignment: Some(LocalName::Single(
                                            "ifTempAssignment".to_string(),
                                        )),
                                        declare_var: true,
                                        condition,
                                        succeed: expr_if
                                            .then_branch
                                            .stmts
                                            .iter()
                                            .map(|stmt| {
                                                handle_stmt(stmt, global_data, current_module)
                                            })
                                            .collect::<Vec<_>>(),
                                        fail: expr_if.else_branch.as_ref().map(|(_, expr)| {
                                            Box::new(handle_expr(
                                                &*expr,
                                                global_data,
                                                current_module,
                                            ))
                                        }),
                                    }),
                                    false,
                                )
                            }
                        }
                        Expr::Match(expr_match) => {
                            if semi.is_some() {
                                handle_stmt(stmt, global_data, current_module)
                            } else {
                                JsStmt::Expr(
                                    handle_expr_match(
                                        expr_match,
                                        true,
                                        global_data,
                                        current_module,
                                    ),
                                    false,
                                )
                            }
                        }
                        _ => handle_stmt(stmt, global_data, current_module),
                    },
                    _ => handle_stmt(stmt, global_data, current_module),
                }
            } else {
                handle_stmt(stmt, global_data, current_module)
            }
        })
        .collect::<Vec<_>>()
}

/// Like handle_expr() but will wrap the result in a `.copy()` if a Expr::Unary for certain cases
///
/// TODO why
fn should_copy_expr_unary(expr: &Expr, global_data: &GlobalData) -> bool {
    // NOTE be careful not to call handle_expr() for any expression that might be Expr::Unary to avoid an infinite loop
    match expr {
        Expr::Unary(expr_unary) => {
            // let copy_stmt = JsStmt::Expr(
            //     JsExpr::MethodCall(
            //         Box::new(handle_expr(
            //             &*expr_unary.expr,
            //             global_data,
            //             current_module,
            //         )),
            //         "copy".to_string(),
            //         Vec::new(),
            //     ),
            //     false,
            // );
            match expr_unary.op {
                UnOp::Deref(_) => match &*expr_unary.expr {
                    Expr::Call(expr_call) => {
                        // TODO for now just assume that any call being dereferenced returns a &mut and should be `.copy()`d. I don't think this will cause any incorrect behavior, only unnecessary copying, eg where the return is `&i32` not `&mut i32`
                        true
                    }
                    Expr::If(_) => todo!(),
                    Expr::Macro(_) => todo!(),
                    Expr::Match(_) => todo!(),
                    Expr::MethodCall(_) => todo!(),
                    Expr::Paren(_) => todo!(),
                    Expr::Path(expr_path) => {
                        if expr_path.path.segments.len() == 1 {
                            global_data.scopes.last().unwrap().0.iter().rev().any(
                                |ScopedVar { name, mut_ref, .. }| {
                                    let lookup_varname =
                                        expr_path.path.segments.first().unwrap().ident.to_string();
                                    *name == lookup_varname && *mut_ref
                                },
                            )
                        } else {
                            todo!()
                        }
                    }
                    Expr::Reference(_) => todo!(),
                    Expr::Unary(_) => todo!(),
                    _ => todo!(),
                },
                _ => todo!(),
            }
        }
        _ => false,
    }
}

fn handle_expr(expr: &Expr, global_data: &mut GlobalData, current_module: &Vec<String>) -> JsExpr {
    match expr {
        Expr::Array(expr_array) => JsExpr::Array(
            expr_array
                .elems
                .iter()
                .map(|elem| handle_expr(elem, global_data, current_module))
                .collect::<Vec<_>>(),
        ),
        Expr::Assign(expr_assign) => {
            let mut rhs = handle_expr(&*expr_assign.right, global_data, current_module);
            if should_copy_expr_unary(&*expr_assign.right, global_data) {
                rhs = JsExpr::MethodCall(Box::new(rhs), "copy".to_string(), Vec::new());
            }
            JsExpr::Assignment(
                Box::new(handle_expr(&*expr_assign.left, global_data, current_module)),
                Box::new(rhs),
            )
        }
        Expr::Async(_) => todo!(),
        Expr::Await(expr_await) => JsExpr::Await(Box::new(handle_expr(
            &*expr_await.base,
            global_data,
            current_module,
        ))),
        Expr::Binary(expr_binary) => {
            // TODO hack to not convert === to .eq() when comparing JS primitives
            let primitive = match &*expr_binary.left {
                Expr::Field(expr_field) => match &expr_field.member {
                    Member::Named(ident) => {
                        ident == "js_number" || ident == "js_string" || ident == "js_bool"
                    }
                    Member::Unnamed(_) => false,
                },
                _ => false,
            };
            if primitive {
                return JsExpr::Binary(
                    Box::new(handle_expr(&*expr_binary.left, global_data, current_module)),
                    JsOp::from_binop(expr_binary.op),
                    Box::new(handle_expr(
                        &*expr_binary.right,
                        global_data,
                        current_module,
                    )),
                );
            }

            let lhs = Box::new(JsExpr::Paren(Box::new(handle_expr(
                &*expr_binary.left,
                global_data,
                current_module,
            ))));
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
            let rhs = handle_expr(&*expr_binary.right, global_data, current_module);
            JsExpr::MethodCall(lhs, camel(method_name), vec![rhs])
        }
        Expr::Block(expr_block) => JsExpr::Block(
            expr_block
                .block
                .stmts
                .iter()
                .map(|stmt| handle_stmt(stmt, global_data, current_module))
                .collect::<Vec<_>>(),
        ),
        Expr::Break(_) => JsExpr::Break,
        Expr::Call(expr_call) => {
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

            // handle tuple structs
            match &*expr_call.func {
                Expr::Path(expr_path) => {
                    let path = expr_path
                        .path
                        .segments
                        .iter()
                        .map(|seg| seg.ident.to_string())
                        .collect::<Vec<_>>();
                    let name = path.last().unwrap();
                    // TODO need to properly identify what is an enum variant and what is a tuple struct. For now assume paths with length 1 are tuple structs
                    if path.len() == 1
                        && name.chars().next().unwrap().is_ascii_uppercase()
                        && name != "Some"
                        && name != "Ok"
                        && name != "Err"
                    {
                        return JsExpr::New(path, args);
                    }
                }
                _ => {}
            }

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

            match &*expr_call.func {
                Expr::Path(expr_path) => {
                    let segments = expr_path
                        .path
                        .segments
                        .iter()
                        .map(|seg| seg.ident.to_string())
                        .collect::<Vec<_>>();

                    if segments.last().unwrap() == "fetch2" {
                        // TODO improve this code
                        JsExpr::FnCall(Box::new(JsExpr::Path(vec!["fetch".to_string()])), args)
                    } else if segments.last().unwrap() == "stringify" {
                        JsExpr::FnCall(
                            Box::new(JsExpr::Path(vec![
                                "JSON".to_string(),
                                "stringify".to_string(),
                            ])),
                            args,
                        )
                    } else if segments.len() == 2 && segments[0] == "Json" && segments[1] == "parse"
                    {
                        JsExpr::FnCall(
                            Box::new(JsExpr::Path(vec!["JSON".to_string(), "parse".to_string()])),
                            args,
                        )
                    } else if segments.len() == 2
                        && segments[0] == "Date"
                        && segments[1] == "from_iso_string"
                    {
                        JsExpr::New(vec!["Date".to_string()], args)
                    } else if segments.len() == 2
                        && segments[0] == "Document"
                        && segments[1] == "query_selector_body"
                    {
                        JsExpr::FnCall(
                            Box::new(JsExpr::Path(vec![
                                "document".to_string(),
                                "querySelector".to_string(),
                            ])),
                            vec![JsExpr::LitStr("body".to_string())],
                        )
                    } else if segments.len() == 2
                        && segments[0] == "Document"
                        && segments[1] == "create_element_div"
                    {
                        JsExpr::FnCall(
                            Box::new(JsExpr::Path(vec![
                                "document".to_string(),
                                "createElement".to_string(),
                            ])),
                            vec![JsExpr::LitStr("div".to_string())],
                        )
                    } else {
                        JsExpr::FnCall(
                            Box::new(handle_expr(&*expr_call.func, global_data, current_module)),
                            args,
                        )
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
                _ => JsExpr::FnCall(
                    Box::new(handle_expr(&*expr_call.func, global_data, current_module)),
                    args,
                ),
            }
        }
        Expr::Cast(_) => todo!(),
        Expr::Closure(expr_closure) => {
            let async_ = match &*expr_closure.body {
                Expr::Async(_) => true,
                _ => false,
            };

            let block = match &*expr_closure.body {
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

            let body = match &*expr_closure.body {
                Expr::Block(expr_block) => {
                    parse_fn_body_stmts(&expr_block.block.stmts, global_data, current_module)
                }
                Expr::Async(expr_async) => {
                    parse_fn_body_stmts(&expr_async.block.stmts, global_data, current_module)
                }
                Expr::Match(expr_match) => {
                    vec![JsStmt::Expr(
                        handle_expr_match(expr_match, true, global_data, current_module),
                        false,
                    )]
                }
                other => vec![JsStmt::Expr(
                    handle_expr(other, global_data, current_module),
                    false,
                )],
            };

            JsExpr::ArrowFn(async_, block, inputs, body)
        }
        Expr::Const(_) => todo!(),
        Expr::Continue(_) => todo!(),
        Expr::Field(expr_field) => {
            let base = Box::new(handle_expr(&*expr_field.base, global_data, current_module));
            match &expr_field.member {
                Member::Named(ident) => JsExpr::Field(base, camel(ident)),
                Member::Unnamed(index) => {
                    JsExpr::Index(base, Box::new(JsExpr::LitInt(index.index as i32)))
                }
            }
        }
        Expr::ForLoop(expr_for_loop) => JsExpr::ForLoop(
            match &*expr_for_loop.pat {
                Pat::Ident(pat_ident) => camel(&pat_ident.ident),
                _ => todo!(),
            },
            Box::new(handle_expr(
                &*expr_for_loop.expr,
                global_data,
                current_module,
            )),
            expr_for_loop
                .body
                .stmts
                .iter()
                .map(|stmt| handle_stmt(&stmt, global_data, current_module))
                .collect::<Vec<_>>(),
        ),
        Expr::Group(_) => todo!(),
        Expr::If(expr_if) => {
            match &*expr_if.cond {
                Expr::Let(_) => todo!(),
                _ => {}
            }
            JsExpr::If(JsIf {
                assignment: None,
                declare_var: false,
                condition: Box::new(JsExpr::Field(
                    Box::new(JsExpr::Paren(Box::new(handle_expr(
                        &*expr_if.cond,
                        global_data,
                        current_module,
                    )))),
                    "jsBoolean".to_string(),
                )),
                succeed: expr_if
                    .then_branch
                    .stmts
                    .iter()
                    .map(|stmt| handle_stmt(stmt, global_data, current_module))
                    .collect::<Vec<_>>(),
                fail: expr_if
                    .else_branch
                    .as_ref()
                    .map(|(_, expr)| Box::new(handle_expr(&*expr, global_data, current_module))),
            })
        }
        Expr::Index(expr_index) => JsExpr::Index(
            Box::new(handle_expr(&*expr_index.expr, global_data, current_module)),
            Box::new(JsExpr::Field(
                Box::new(handle_expr(&*expr_index.index, global_data, current_module)),
                "jsNumber".to_string(),
            )),
        ),
        Expr::Infer(_) => todo!(),
        Expr::Let(expr_let) => {
            dbg!(expr_let);
            todo!()
        }
        Expr::Lit(expr_lit) => match &expr_lit.lit {
            Lit::Str(lit_str) => {
                global_data.rust_prelude_types.string = true;
                JsExpr::New(
                    vec!["RustString".to_string()],
                    vec![JsExpr::LitStr(lit_str.value())],
                )
            }
            Lit::ByteStr(_) => todo!(),
            Lit::Byte(_) => todo!(),
            Lit::Char(_) => todo!(),
            Lit::Int(lit_int) => {
                global_data.rust_prelude_types.integer = true;
                JsExpr::New(
                    vec!["RustInteger".to_string()],
                    vec![JsExpr::LitInt(lit_int.base10_parse::<i32>().unwrap())],
                )
            }
            Lit::Float(lit_float) => {
                global_data.rust_prelude_types.float = true;
                JsExpr::New(
                    vec!["RustFloat".to_string()],
                    vec![JsExpr::LitFloat(lit_float.base10_parse::<f32>().unwrap())],
                )
            }
            Lit::Bool(lit_bool) => {
                global_data.rust_prelude_types.bool = true;
                JsExpr::New(
                    vec!["RustBool".to_string()],
                    vec![JsExpr::LitBool(lit_bool.value)],
                )
            }
            Lit::Verbatim(_) => todo!(),
            _ => todo!(),
        },
        Expr::Loop(expr_loop) => JsExpr::While(
            Box::new(JsExpr::LitBool(true)),
            expr_loop
                .body
                .stmts
                .iter()
                .map(|stmt| handle_stmt(stmt, global_data, current_module))
                .collect::<Vec<_>>(),
        ),
        Expr::Macro(expr_macro) => {
            // TODO share this code with Stmt::Macro
            let path_segs = expr_macro
                .mac
                .path
                .segments
                .iter()
                .map(|seg| seg.ident.to_string())
                .collect::<Vec<_>>();

            // TODO update this to output Vec not []
            if path_segs.len() == 1 {
                if path_segs[0] == "vec" {
                    let input = expr_macro.mac.tokens.clone().to_string();
                    let expr_array =
                        syn::parse_str::<syn::ExprArray>(&format!("[{input}]")).unwrap();
                    let expr_vec = expr_array
                        .elems
                        .iter()
                        .map(|elem| handle_expr(elem, global_data, current_module))
                        .collect::<Vec<_>>();
                    return JsExpr::Array(expr_vec);
                }
            }

            let stmt_macro = expr_macro;
            let current_module_path = current_module;

            if path_segs.len() == 1 {
                if path_segs[0] == "panic" {
                    let input = stmt_macro.mac.tokens.clone().to_string();
                    // TODO use a custom error so it isn't inadvertently caught by other JS code in the app?
                    return JsExpr::ThrowError(input);
                }
                // if path_segs[0] == "try_" {
                //     let input = stmt_macro.mac.tokens.clone().to_string();
                //     let try_block = syn::parse_str::<syn::Block>(&input).unwrap();
                //     let stmt_vec = try_block
                //         .stmts
                //         .iter()
                //         .map(|stmt| handle_stmt(stmt, global_data, current_module_path))
                //         .collect::<Vec<_>>();
                //     return JsStmt::TryBlock(stmt_vec);
                // }
                // if path_segs[0] == "catch" {
                //     let input = stmt_macro.mac.tokens.clone().to_string();
                //     let mut parts = input.split(",");
                //     let err_var_name = parts.next().unwrap();
                //     let err_var_name = syn::parse_str::<syn::Ident>(err_var_name)
                //         .unwrap()
                //         .to_string();
                //     let _err_var_type = parts.next().unwrap();
                //     let catch_block = parts.collect::<String>();
                //     let catch_block = syn::parse_str::<syn::Block>(&catch_block).unwrap();
                //     let stmt_vec = catch_block
                //         .stmts
                //         .into_iter()
                //         .map(|stmt| handle_stmt(&stmt, global_data, current_module_path));
                //     let stmt_vec = stmt_vec.collect::<Vec<_>>();
                //     return JsStmt::CatchBlock(err_var_name, stmt_vec);
                // }
                if path_segs[0] == "assert" {
                    let input = stmt_macro.mac.tokens.clone().to_string();
                    let condition_expr = syn::parse_str::<syn::Expr>(&input).unwrap();
                    let condition_js = JsExpr::Field(
                        Box::new(JsExpr::Paren(Box::new(handle_expr(
                            &condition_expr,
                            global_data,
                            current_module_path,
                        )))),
                        "jsBoolean".to_string(),
                    );

                    return JsExpr::MethodCall(
                        Box::new(JsExpr::Path(vec!["console".to_string()])),
                        "assert".to_string(),
                        vec![condition_js],
                    );
                }
                if path_segs[0] == "assert_eq" {
                    let input = stmt_macro.mac.tokens.clone().to_string();
                    let mut parts = input.split(",");

                    let lhs = parts.next().unwrap();
                    let lhs = syn::parse_str::<syn::Expr>(lhs).unwrap();
                    let lhs = handle_expr(&lhs, global_data, current_module_path);

                    let rhs = parts.next().unwrap();
                    let rhs = syn::parse_str::<syn::Expr>(rhs).unwrap();
                    let rhs = handle_expr(&rhs, global_data, current_module_path);

                    // let equality_check = JsExpr::Binary(Box::new(lhs), JsOp::Eq, Box::new(rhs));
                    let equality_check = JsExpr::Field(
                        Box::new(JsExpr::Paren(Box::new(JsExpr::MethodCall(
                            Box::new(lhs),
                            "eq".to_string(),
                            vec![rhs],
                        )))),
                        "jsBoolean".to_string(),
                    );
                    return JsExpr::MethodCall(
                        Box::new(JsExpr::Path(vec!["console".to_string()])),
                        "assert".to_string(),
                        vec![equality_check],
                    );
                }
            }
            todo!()
        }
        Expr::Match(expr_match) => {
            handle_expr_match(expr_match, false, global_data, current_module)
        }
        Expr::MethodCall(expr_method_call) => {
            let mut method_name = expr_method_call.method.to_string();
            let receiver = handle_expr(&*expr_method_call.receiver, global_data, current_module);

            if let JsExpr::LitStr(_) = receiver {
                if method_name == "to_string" {
                    return receiver;
                }
            }
            if let JsExpr::Path(path) = &receiver {
                if path.len() == 2 {
                    if path[0] == "JSON" && path[1] == "parse" {
                        // function parse(text) {try { return Result.Ok(JSON.parse(text)); } catch(err) { return Result.Err(err) }}
                        let body = "try { return Result.Ok(JSON.parse(text)); } catch(err) { return Result.Err(err) }".to_string();
                        return JsExpr::FnCall(
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
                                .map(|arg| handle_expr(arg, global_data, current_module))
                                .collect::<Vec<_>>(),
                        );
                    }
                }
            }
            if method_name == "iter" {
                return receiver;
            }
            if method_name == "collect" {
                return receiver;
            }
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
                return JsExpr::Field(Box::new(receiver), "length".to_string());
            }
            JsExpr::MethodCall(
                Box::new(receiver),
                camel(method_name),
                expr_method_call
                    .args
                    .iter()
                    .map(|arg| handle_expr(arg, global_data, current_module))
                    .collect::<Vec<_>>(),
            )
        }
        Expr::Paren(expr_paren) => JsExpr::Paren(Box::new(handle_expr(
            &*expr_paren.expr,
            global_data,
            current_module,
        ))),
        Expr::Path(expr_path) => {
            let mut segs = expr_path
                .path
                .segments
                .iter()
                .map(|seg| {
                    let mut var_name = seg.ident.to_string();
                    if var_name == "Document" {
                        var_name = "document".to_string();
                    }
                    if var_name == "Console" {
                        var_name = "console".to_string();
                    }
                    // TODO be more targetted with this
                    if let Some(last_char) = var_name.chars().last() {
                        if last_char.is_digit(10) {
                            var_name.pop().unwrap();
                        }
                    }
                    // case_convert(var_name)
                    var_name
                })
                .collect::<Vec<_>>();
            if segs.len() == 1 {
                // if segs[0] == "None" {
                //     return JsExpr::Null;
                // }
                if segs[0] == "None" {
                    global_data.rust_prelude_types.option = true;
                    global_data.rust_prelude_types.none = true;
                }
                if segs[0] == "self" {
                    segs[0] = "this".to_string();
                }
            }
            if global_data.snippet {
                return JsExpr::Path(segs.iter().map(|seg| case_convert(seg)).collect::<Vec<_>>());
            }

            // So we have a path like foo::bar::baz()
            // The algorithm for finding the full module path to the item is:
            // Iif segs[0] is crate, super, or self, then jump to that module
            // else
            // Look to see if segs[0] is defined in any other the parent scopes
            // else
            // Look to see if segs[0] is defined at the module level
            // else
            // Look to see if segs[0] is a child module
            // else
            // Look to see if segs[0] is used

            // TODO:
            // handle mixed paths eg submodule -> use
            // make sure `mod` being pub/private is taken into account - this would only be for use paths since mod is always public in the file it is called from (parent), so would happen in the `use` resolving step

            // Take a path segs like foo::my_func(), and finds the absolute path to the item eg crate::bar::foo::my_func()
            // Actually it should find the path relative to the seed path ie current_module, which is why it is useful to use recursively and for resolving use paths???
            fn get_path(
                use_private_items: bool,
                // So we know whether allow segs to simply be somthing in an outer scope
                module_level_items_only: bool,
                module: &ModuleData,
                mut segs: Vec<String>,
                global_data: &GlobalData,
                current_module: &Vec<String>,
                // Only used to determine if current module is
                original_module: &Vec<String>,
            ) -> Vec<String> {
                // TODO All parent modules are visible/public to their desecendants. When parents are accessed via `super`, it is easy the flag `use_private_items = true`. However when modules are accessed via `crate:: ...` I don't think there is any way to know whether the path leads a module which is a parent of (or is) the original module (ie so should be public), so we need to do this check. No - even if we access an item via `crate`, once we then visit a submodule, `use_private_items` get set to false, the problem is actually that sometimes we will want it to be true, when crate::submodule is actually a parent of the original module. So really we should just keep `is_parent_or_same_module` but a more efficient approach is to use `use_private_items` for crate, super, self, etc, then only calculate `is_parent_or_same_module` for submodule and pass as use_private_items
                let is_parent_or_same_module = if original_module.len() >= current_module.len() {
                    current_module
                        .iter()
                        .enumerate()
                        .all(|(i, current_module)| current_module == &original_module[i])
                } else {
                    false
                };

                let item_defined_in_module = module.item_defined_in_module(
                    use_private_items || is_parent_or_same_module,
                    &segs[0],
                );

                // Whilst the immediate child modules are public to all modules (not their items, but the module itself), we might not actually be accessing it directly from the parent eg `foo::bar::baz()` and `bar` is not `pub`
                let path_starts_with_sub_module = module.path_starts_with_sub_module(
                    use_private_items || is_parent_or_same_module,
                    &segs[0],
                );

                // We are looking for the origin of some name so we want to compare that name with the item the use statement is importing
                // If we have a match then use want to add the use path to the path of the current module, eg:
                // current_module::crate::foo:item - we can overwrite the current module with "crate"
                // current_module::super::foo:item - we can pop the last seg of the current module
                // current_module::submodule::foo::item - add the submodule to the current module
                // current_module::another_used_item::item - we need to get the path of this subsequent used item in the same way described here, so we are recursing, then return to the original use path and carry on adding the rest of the segments
                let mut use_mappings = module.pub_use_mappings.iter();
                let matched_use_mapping = if use_private_items || is_parent_or_same_module {
                    use_mappings
                        .chain(module.private_use_mappings.iter())
                        .find(|use_mapping| use_mapping.0 == segs[0])
                } else {
                    use_mappings.find(|use_mapping| use_mapping.0 == segs[0])
                };
                // let path_starts_with_a_used_item_or_mod = module
                //     .resolved_mappings
                //     .iter()
                //     .find(|use_mapping| use_mapping.0 == segs[0]);

                // dbg!(&segs);
                // dbg!(&module);
                // dbg!(&current_module);
                // dbg!(&use_private_items);
                // dbg!(&module_level_items_only);
                // dbg!(&module
                //     .pub_submodules
                //     .iter()
                //     .chain(module.private_submodules.iter()));
                // dbg!(module.private_definitions.contains(&segs[0]));
                // println!("");
                // dbg!(&module.resolved_mappings);
                // dbg!(&segs[0]);

                if item_defined_in_module {
                    // Path starts with an item defined in the module

                    // Check whether it is not globally unique and so has been namespaced
                    if let Some(dup) = global_data.duplicates.iter().find(|dup| {
                        dup.name == segs[0] && &dup.original_module_path == current_module
                    }) {
                        segs[0] = dup
                            .namespace
                            .iter()
                            .map(|seg| camel(seg))
                            .collect::<Vec<_>>()
                            .join("__");
                    }

                    // dbg!("item defined in module");

                    segs
                } else if segs[0] == "super" {
                    // TODO if a module level item name is shadowed by a item in a fn scope, then module level item needs to be namespaced
                    segs.remove(0);

                    let mut current_module = current_module.clone();
                    current_module.pop();

                    let module = global_data
                        .modules
                        .iter()
                        .find(|module| module.path == current_module)
                        .unwrap();

                    // dbg!("in super");
                    get_path(
                        true,
                        true,
                        module,
                        segs,
                        global_data,
                        &current_module,
                        original_module,
                    )
                } else if segs[0] == "self" {
                    // NOTE private items are still accessible from the module via self
                    segs.remove(0);

                    // I believe this works because the only effect of self is to look for the item only at the module level, rather than up through the fn scopes first, so get_path without the self and `in_same_module = false` achieves this, including handling any subsequent `super`s
                    // TODO problem is that we are conflating `in_same_module` with pub/private
                    // dbg!("in self");
                    get_path(
                        true,
                        true,
                        module,
                        segs,
                        global_data,
                        &current_module,
                        original_module,
                    )
                } else if segs[0] == "crate" {
                    let current_module = vec!["crate".to_string()];
                    let module = global_data
                        .modules
                        .iter()
                        .find(|module| module.path == current_module)
                        .unwrap();

                    // TODO descendants can access private items, so it depends whether the module trying to access the item is a descendant of the module it item is in, so need to keep track of the original call site?

                    // dbg!("in crate");
                    segs.remove(0);

                    // NOTE all modules are desecendants of crate so all items in crate are visible/public
                    get_path(
                        true,
                        true,
                        module,
                        segs,
                        global_data,
                        &current_module,
                        original_module,
                    )
                } else if path_starts_with_sub_module {
                    // Path starts with a submodule of the current module
                    let mut submodule_path = current_module.clone();
                    submodule_path.push(segs[0].clone());

                    let submodule = global_data
                        .modules
                        .iter()
                        .find(|submodule| submodule.path == submodule_path)
                        .unwrap();

                    // If we are going to be looking within the submodule that the current path starts with, we should remove it from the path since get_path() doesn't expect paths to a module to start with their name since this is not valid Rust, and self must be used instead.
                    segs.remove(0);

                    // dbg!("Path starts with a submodule of the current module");
                    get_path(
                        false,
                        true,
                        submodule,
                        segs,
                        global_data,
                        &submodule_path,
                        original_module,
                    )
                } else if let Some(use_mapping) = matched_use_mapping {
                    let mut use_segs = use_mapping.1.clone();
                    use_segs.push(use_mapping.0.clone());
                    segs.remove(0);
                    use_segs.extend(segs);
                    let mut segs = get_path(
                        true,
                        true,
                        module,
                        use_segs,
                        global_data,
                        current_module,
                        original_module,
                    );

                    if let Some(dup) = global_data.duplicates.iter().find(|dup| {
                        dup.name == use_mapping.0 && dup.original_module_path == use_mapping.1
                    }) {
                        // If the item has been namespaced, we need to replace it with the namespace
                        segs[0] = dup
                            .namespace
                            .iter()
                            .map(|seg| camel(seg))
                            .collect::<Vec<_>>()
                            .join("__");
                        segs
                    } else {
                        // If the item has not been namespaced, we don't need to do anything
                        segs
                    }
                } else if segs.len() == 1 && segs[0] == "this" {
                    // dbg!("in this");
                    segs
                } else if use_private_items {
                    // If none of the above conditions are met then assume the path refers to an item/variable in an enclosing scope within this module, not at the top level of a module, so simply return the path
                    // dbg!("in private items");
                    segs
                } else {
                    dbg!(module);
                    dbg!(current_module);
                    dbg!(segs);
                    panic!()
                }
            }

            // dbg!(&global_data.modules);
            // dbg!(&current_module);
            let module = global_data
                .modules
                .iter()
                .find(|module| &module.path == current_module)
                .unwrap();

            // let segs = get_path(true, module, segs, global_data, current_module)
            //     .iter()
            //     .map(|seg| case_convert(seg))
            //     .collect::<Vec<_>>();

            let mut segs = get_path(
                true,
                false,
                module,
                segs,
                global_data,
                current_module,
                current_module,
            );

            // convert case of rest of path
            for (i, seg) in segs.iter_mut().enumerate() {
                if i == 0 && seg.contains("__") {
                    // namespaced item already case converted
                } else {
                    *seg = case_convert(seg.clone())
                }
            }

            JsExpr::Path(segs)
        }
        Expr::Range(_) => todo!(),
        Expr::Reference(expr_reference) => {
            handle_expr(&*expr_reference.expr, global_data, current_module)
        }
        Expr::Repeat(_) => todo!(),
        Expr::Return(expr_return) => {
            if let Some(expr) = &expr_return.expr {
                // If return is the deref of a &mut, then `.copy()` it
                let js_expr = match &**expr {
                    Expr::Unary(expr_unary) => match expr_unary.op {
                        UnOp::Deref(_) => JsExpr::MethodCall(
                            Box::new(handle_expr(expr, global_data, current_module)),
                            "copy".to_string(),
                            Vec::new(),
                        ),
                        _ => handle_expr(expr, global_data, current_module),
                    },
                    _ => handle_expr(expr, global_data, current_module),
                };

                JsExpr::Return(Box::new(js_expr))
            } else {
                // TODO surely an empty in Rust should also be an empty return in JS?
                JsExpr::Return(Box::new(JsExpr::Vanish))
            }
        }
        Expr::Struct(expr_struct) => {
            let segs = expr_struct
                .path
                .segments
                .iter()
                .map(|seg| seg.ident.to_string())
                .collect::<Vec<_>>();
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
                            Box::new(handle_expr(&field.expr, global_data, current_module)),
                        )
                    })
                    .collect::<Vec<_>>(),
            );
            if segs.len() == 2 {
                JsExpr::FnCall(Box::new(JsExpr::Path(segs)), vec![obj])
            } else {
                let struct_name = segs.first().unwrap().clone();
                if struct_name == "FetchOptions" || struct_name == "SseOptions" {
                    obj
                } else {
                    // TODO we are assuming all other struct literals are inside ::new() so can be disappeared because the JsClass will write the constructor body
                    // JsExpr::Vanish
                    // TODO Expr structs can be instaniating an object but also instantiating an enum Variant with struct args. For now assume all Paths with len == 2 are enum variants and everthing else is a struct instaniation. Need an improved AST.
                    let args = expr_struct
                        .fields
                        .iter()
                        .map(|field| handle_expr(&field.expr, global_data, current_module))
                        .collect::<Vec<_>>();

                    JsExpr::New(vec![struct_name], args)
                }
            }
        }
        Expr::Try(_) => todo!(),
        Expr::TryBlock(_) => todo!(),
        Expr::Tuple(_) => todo!(),
        Expr::Unary(expr_unary) => match expr_unary.op {
            UnOp::Deref(_) => handle_expr(&*expr_unary.expr, global_data, current_module),
            UnOp::Not(_) => JsExpr::Not(Box::new(handle_expr(
                &*expr_unary.expr,
                global_data,
                current_module,
            ))),
            UnOp::Neg(_) => JsExpr::Minus(Box::new(handle_expr(
                &*expr_unary.expr,
                global_data,
                current_module,
            ))),
            _ => todo!(),
        },
        Expr::Unsafe(_) => todo!(),
        Expr::Verbatim(_) => todo!(),
        Expr::While(expr_while) => JsExpr::While(
            Box::new(handle_expr(&*expr_while.cond, global_data, current_module)),
            expr_while
                .body
                .stmts
                .iter()
                .map(|stmt| handle_stmt(stmt, global_data, current_module))
                .collect::<Vec<_>>(),
        ),
        Expr::Yield(_) => todo!(),
        _ => todo!(),
    }
}

/// Get match pattern ident to be used as rhs of if conditions like `myData.id === MyEnum.fooId`, and start a body stmts Vec to with any pattern arg destructuring that might be necessary
///
/// (rhs, start of body Vec)
fn handle_match_pat(
    arm_pat: &Pat,
    expr_match: &ExprMatch,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> (Vec<String>, Vec<JsStmt>) {
    match arm_pat {
        Pat::Const(_) => todo!(),
        Pat::Ident(pat_ident) => {
            let empty_vec: Vec<JsStmt> = Vec::new();
            let ident = pat_ident.ident.to_string();
            (vec![ident], empty_vec)
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
            let stmt = JsStmt::Local(JsLocal {
                public: false,
                export: false,
                type_: LocalType::Var,
                lhs: LocalName::DestructureObject(DestructureObject(names)),
                value: JsExpr::Field(
                    Box::new(handle_expr(&*expr_match.expr, global_data, current_module)),
                    "data".to_string(),
                ),
            });
            let rhs = pat_struct
                .path
                .segments
                .iter()
                .map(|seg| seg.ident.to_string())
                .collect::<Vec<_>>();
            (rhs, vec![stmt])
        }
        Pat::Tuple(_) => todo!(),
        Pat::TupleStruct(pat_tuple_struct) => {
            let names = pat_tuple_struct
                .elems
                .iter()
                .map(|elem| handle_pat(elem))
                .collect::<Vec<_>>();
            let stmt = JsStmt::Local(JsLocal {
                public: false,
                export: false,
                type_: LocalType::Var,
                lhs: LocalName::DestructureArray(names),
                value: JsExpr::Field(
                    Box::new(handle_expr(&*expr_match.expr, global_data, current_module)),
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
            )
        }
        Pat::Type(_) => todo!(),
        Pat::Verbatim(_) => todo!(),
        Pat::Wild(_) => todo!(),
        _ => todo!(),
    }
}

fn handle_expr_match(
    expr_match: &ExprMatch,
    is_returned: bool,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> JsExpr {
    // (assignment, condition, succeed, fail)
    // TODO we need to know whether match result is being assigned to a var and therefore the if statement should be adding assignments to the end of each block

    // Fold match arms into if else statements
    let if_expr = expr_match.arms.iter().rev().fold(
        JsExpr::ThrowError("couldn't match enum variant".to_string()),
        |acc, arm| {
            let (mut rhs, mut body_data_destructure) =
                handle_match_pat(&arm.pat, expr_match, global_data, current_module);

            if rhs == ["Some"] || rhs == ["None"] {
                rhs.insert(0, "Option".to_string());
            }

            // Need to take the path which will be eg [MyEnum, Baz], and convert to [MyEnum.bazId]
            let index = rhs.len() - 1;
            // dbg!(rhs);
            // todo!();
            // if rhs[0] == "Option" {
            //     rhs = rhs[1..].to_vec();
            // }
            rhs[index] = format!("{}Id", camel(rhs[index].clone()));

            let body = match &*arm.body {
                // Expr::Array(_) => [JsStmt::Raw("sdafasdf".to_string())].to_vec(),
                Expr::Array(_) => vec![JsStmt::Raw("sdafasdf".to_string())],
                Expr::Block(expr_block) => expr_block
                    .block
                    .stmts
                    .iter()
                    .map(|stmt| handle_stmt(stmt, global_data, current_module))
                    .collect::<Vec<_>>(),
                other_expr => {
                    vec![JsStmt::Expr(
                        handle_expr(other_expr, global_data, current_module),
                        false,
                    )]
                }
            };
            body_data_destructure.extend(body.into_iter());
            let body = body_data_destructure;

            JsExpr::If(JsIf {
                assignment: is_returned
                    .then_some(LocalName::Single("ifTempAssignment".to_string())),
                declare_var: is_returned,
                condition: Box::new(JsExpr::Binary(
                    Box::new(JsExpr::Field(
                        Box::new(handle_expr(&*expr_match.expr, global_data, current_module)),
                        "id".to_string(),
                    )),
                    JsOp::Eq,
                    Box::new(JsExpr::Path(rhs)),
                )),
                succeed: body,
                // TODO
                fail: Some(Box::new(acc)),
            })
        },
    );
    // for arm in &expr_match.arms {
    //     dbg!(arm);
    // }
    // todo!()
    if_expr
}

pub fn generate_js_from_module(js: impl ToString) -> String {
    from_module(js.to_string().as_str(), false)
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn format_js(js: impl ToString) -> String {
    let parse = biome_js_parser::parse_script(js.to_string().as_str(), JsParserOptions::default());
    let stmt = parse.syntax().children().nth(1).unwrap();
    let opts = JsFormatOptions::new(JsFileSource::default())
        // .with_indent_width(IndentWidth::from(1))
        .with_indent_style(IndentStyle::Space);
    let formatted_js = biome_formatter::format_node(&stmt, JsFormatLanguage::new(opts)).unwrap();
    formatted_js.print().unwrap().as_code().to_string()
}
