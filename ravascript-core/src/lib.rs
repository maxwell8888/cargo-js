use heck::{AsKebabCase, AsLowerCamelCase, AsPascalCase};
use std::{
    fmt::Debug,
    fs,
    path::{Path, PathBuf},
};
use syn::{
    parse_macro_input, BinOp, DeriveInput, Expr, ExprBlock, ExprMatch, FnArg, ImplItem, Item,
    ItemEnum, ItemFn, ItemMod, ItemUse, Lit, Member, Meta, Pat, Stmt, Type, UnOp, UseTree,
    Visibility,
};

// TODO need to handle expressions which return `()`. Probably use `undefined` for `()` since that is what eg console.log();, var x = 5;, etc returns;
// TODO preserve new lines so generated js is more readable
// TODO consider how to get RA/cargo check to analyze rust inputs in `testing/`
// TODO add assertions to output JS and run that JS to ensure assertions pass

const SSE_RAW_FUNC: &str = r##" function Sse (url, options) {
    if (!(this instanceof Sse)) {
        return new Sse(url, options);
    }

    this.INITIALIZING = -1;
    this.CONNECTING = 0;
    this.OPEN = 1;
    this.CLOSED = 2;

    this.url = url;

    options = options || {};
    this.headers = options.headers || {};
    this.payload = options.payload !== undefined ? options.payload : "";
    this.method = options.method || (this.payload && "POST") || "GET";
    this.withCredentials = !!options.withCredentials;
    this.debug = !!options.debug;

    this.FIELD_SEPARATOR = ":";

    this.listeners = {};

    this.xhr = null;
    this.readyState = this.INITIALIZING;
    this.progress = 0;
    this.chunk = "";

    this.addEventListener = function (type, listener) {
        if (this.listeners[type] === undefined) {
            this.listeners[type] = [];
        }

        if (this.listeners[type].indexOf(listener) === -1) {
            this.listeners[type].push(listener);
        }
    };

    this.removeEventListener = function (type, listener) {
        if (this.listeners[type] === undefined) {
            return;
        }

        var filtered = [];
        this.listeners[type].forEach(function (element) {
            if (element !== listener) {
                filtered.push(element);
            }
        });
        if (filtered.length === 0) {
            delete this.listeners[type];
        } else {
            this.listeners[type] = filtered;
        }
    };

    this.dispatchEvent = function (e) {
        if (!e) {
            return true;
        }

        if (this.debug) {
            console.debug(e);
        }

        e.source = this;

        var onHandler = "on" + e.type;
        if (this.hasOwnProperty(onHandler)) {
            this[onHandler].call(this, e);
            if (e.defaultPrevented) {
                return false;
            }
        }

        if (this.listeners[e.type]) {
            return this.listeners[e.type].every(function (callback) {
                callback(e);
                return !e.defaultPrevented;
            });
        }

        return true;
    };

    this._setReadyState = function (state) {
        var event = new CustomEvent("readystatechange");
        event.readyState = state;
        this.readyState = state;
        this.dispatchEvent(event);
    };

    this._onStreamFailure = function (e) {
        var event = new CustomEvent("error");
        event.data = e.currentTarget.response;
        this.dispatchEvent(event);
        this.close();
    };

    this._onStreamAbort = function (e) {
        this.dispatchEvent(new CustomEvent("abort"));
        this.close();
    };

    this._onStreamProgress = function (e) {
        if (!this.xhr) {
            return;
        }

        if (this.xhr.status !== 200) {
            this._onStreamFailure(e);
            return;
        }

        if (this.readyState == this.CONNECTING) {
            this.dispatchEvent(new CustomEvent("open"));
            this._setReadyState(this.OPEN);
        }

        var data = this.xhr.responseText.substring(this.progress);

        this.progress += data.length;
        var parts = (this.chunk + data).split(/(\r\n|\r|\n){2}/g);

        var lastPart = parts.pop();
        parts.forEach(
            function (part) {
                if (part.trim().length > 0) {
                    this.dispatchEvent(this._parseEventChunk(part));
                }
            }.bind(this)
        );
        this.chunk = lastPart;
    };

    this._onStreamLoaded = function (e) {
        this._onStreamProgress(e);

        this.dispatchEvent(this._parseEventChunk(this.chunk));
        this.chunk = "";
    };

    this._parseEventChunk = function (chunk) {
        if (!chunk || chunk.length === 0) {
            return null;
        }

        if (this.debug) {
            console.debug(chunk);
        }

        var e = { id: null, retry: null, data: null, event: "message" };
        chunk.split(/\n|\r\n|\r/).forEach(
            function (line) {
                line = line.trimRight();
                var index = line.indexOf(this.FIELD_SEPARATOR);
                if (index <= 0) {
                    return;
                }

                var field = line.substring(0, index);
                if (!(field in e)) {
                    return;
                }

                var skip = line[index + 1] === " " ? 2 : 1;
                var value = line.substring(index + skip);

                if (field === "data" && e[field] !== null) {
                    e["data"] += "\n" + value;
                } else {
                    e[field] = value;
                }
            }.bind(this)
        );

        var event = new CustomEvent(e.event);
        event.data = e.data || "";
        event.id = e.id;
        return event;
    };

    this._checkStreamClosed = function () {
        if (!this.xhr) {
            return;
        }

        if (this.xhr.readyState === XMLHttpRequest.DONE) {
            this._setReadyState(this.CLOSED);
        }
    };

    this.stream = function () {
        if (this.xhr) {
            return;
        }

        this._setReadyState(this.CONNECTING);

        this.xhr = new XMLHttpRequest();
        this.xhr.addEventListener("progress", this._onStreamProgress.bind(this));
        this.xhr.addEventListener("load", this._onStreamLoaded.bind(this));
        this.xhr.addEventListener("readystatechange", this._checkStreamClosed.bind(this));
        this.xhr.addEventListener("error", this._onStreamFailure.bind(this));
        this.xhr.addEventListener("abort", this._onStreamAbort.bind(this));
        this.xhr.open(this.method, this.url);
        for (var header in this.headers) {
            this.xhr.setRequestHeader(header, this.headers[header]);
        }
        this.xhr.withCredentials = this.withCredentials;
        this.xhr.send(this.payload);
    };

    this.close = function () {
        if (this.readyState === this.CLOSED) {
            return;
        }

        this.xhr.abort();
        this.xhr = null;
        this._setReadyState(this.CLOSED);
    };

    if (options.start === undefined || options.start) {
        this.stream();
    }
}"##;

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
        UseTree::Glob(_) => todo!(),
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

/// CONST_NAMES -> constNames
/// PascalCase -> PascalCase
/// snake_case -> snakeCase
fn case_convert(name: impl ToString) -> String {
    let name = name.to_string();
    if name.chars().all(|c| c.is_uppercase() || c == '_') {
        camel(name)
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
    let underscore_prefix = text.starts_with("_");
    let camel = AsLowerCamelCase(text).to_string();
    if underscore_prefix {
        format!("_{camel}")
    } else {
        camel
    }
}

fn handle_stmt(stmt: &Stmt, global_data: &mut GlobalData, current_module: &Vec<String>) -> JsStmt {
    match stmt {
        Stmt::Expr(expr, closing_semi) => JsStmt::Expr(
            handle_expr(expr, global_data, current_module),
            closing_semi.is_some(),
        ),
        Stmt::Local(local) => {
            //
            let lhs = match &local.pat {
                Pat::Ident(pat_ident) => LocalName::Single(camel(&pat_ident.ident)),
                Pat::Tuple(pat_tuple) => LocalName::DestructureArray(
                    pat_tuple
                        .elems
                        .iter()
                        .map(|elem| match elem {
                            Pat::Ident(pat_ident) => camel(&pat_ident.ident),
                            _ => todo!(),
                        })
                        .collect::<Vec<_>>(),
                ),
                other => {
                    dbg!(other);
                    todo!()
                }
            };
            let value = handle_expr(
                &*local.init.as_ref().unwrap().expr,
                global_data,
                current_module,
            );
            match value {
                JsExpr::If(_assignment, _declare_var, condition, succeed, fail) => {
                    // TODO currently cases where the branch scope has a var with the same name as the result var means that the result will get assigned to that var, not the result var. Need to consider how to handle this. putting the branch lines inside a new `{}` scope and then doing the result assignment outside of this would work, but is ugly so would want to only do it where necessary, which would require iterating over the lines in a block to check for local declarations with that name.
                    JsStmt::Expr(JsExpr::If(Some(lhs), true, condition, succeed, fail), true)
                }

                value => JsStmt::Local(JsLocal {
                    public: false,
                    export: false,
                    type_: LocalType::Var,
                    lhs,
                    value,
                }),
            }
        }
        Stmt::Item(item) => match item {
            // TODO this should all be handled by `fn handle_item()`
            Item::Const(_) => todo!(),
            Item::Enum(item_enum) => {
                handle_item_enum(item_enum.clone(), global_data, current_module)
            }
            Item::ExternCrate(_) => todo!(),
            Item::Fn(item_fn) => handle_item_fn(item_fn, global_data, current_module),
            Item::ForeignMod(_) => todo!(),
            Item::Impl(_) => JsStmt::Expr(JsExpr::Vanish, false),
            Item::Macro(_) => todo!(),
            Item::Mod(_) => todo!(),
            Item::Static(_) => todo!(),
            // Item::Struct(_) => JsStmt::Expr(JsExpr::Vanish, false),
            Item::Struct(_) => todo!(),
            Item::Trait(_) => todo!(),
            Item::TraitAlias(_) => todo!(),
            Item::Type(_) => todo!(),
            Item::Union(_) => todo!(),
            // Item::Use(item_use) => handle_item_use(item_use),
            Item::Use(item_use) => todo!(),
            Item::Verbatim(_) => todo!(),
            _ => todo!(),
        },
        Stmt::Macro(stmt_macro) => {
            // dbg!(stmt_macro);
            let path_segs = stmt_macro
                .mac
                .path
                .segments
                .iter()
                .map(|seg| seg.ident.to_string())
                .collect::<Vec<_>>();
            if path_segs.len() == 1 {
                if path_segs[0] == "try_" {
                    let input = stmt_macro.mac.tokens.clone().to_string();
                    let try_block = syn::parse_str::<syn::Block>(&input).unwrap();
                    let stmt_vec = try_block
                        .stmts
                        .iter()
                        .map(|stmt| handle_stmt(stmt, global_data, current_module))
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
                        .map(|stmt| handle_stmt(&stmt, global_data, current_module));
                    let stmt_vec = stmt_vec.collect::<Vec<_>>();
                    return JsStmt::CatchBlock(err_var_name, stmt_vec);
                }
                if path_segs[0] == "assert_eq" {
                    let input = stmt_macro.mac.tokens.clone().to_string();
                    let mut parts = input.split(",");

                    let lhs = parts.next().unwrap();
                    let lhs = syn::parse_str::<syn::Expr>(lhs).unwrap();
                    let lhs = handle_expr(&lhs, global_data, current_module);

                    let rhs = parts.next().unwrap();
                    let rhs = syn::parse_str::<syn::Expr>(rhs).unwrap();
                    let rhs = handle_expr(&rhs, global_data, current_module);

                    return JsStmt::Expr(
                        JsExpr::MethodCall(
                            Box::new(JsExpr::Path(vec!["assert".to_string()])),
                            "strictEqual".to_string(),
                            vec![lhs, rhs],
                        ),
                        stmt_macro.semi_token.is_some(),
                    );
                }
            }
            todo!()
        }
    }
}

fn handle_item_fn(
    item_fn: &ItemFn,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> JsStmt {
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
    let mut name = item_fn.sig.ident.to_string();
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
    }
    if ignore {
        JsStmt::Expr(JsExpr::Vanish, false)
    } else {
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
            body_stmts: item_fn
                .block
                .stmts
                .iter()
                .map(|stmt| handle_stmt(stmt, global_data, current_module))
                .collect::<Vec<_>>(),
        };
        if iife {
            JsStmt::Expr(JsExpr::Fn(js_fn), true)
        } else {
            JsStmt::Function(js_fn)
        }
        // JsStmt::Function(js_fn)
    }
}

/// We convert enum variants like Foo::Bar to Foo.bar because otherwise when the variant has arguments, syn is not able to distinguish it from an associated method, so we cannot deduce when Pascal or Camel case should be used, so stick to Pascal for both case.
/// We must store separate <variant name>Id fields because otherwise we end up in a situation where a variable containing an enum variant only contains the data returned the the method with that name and then we can't do myVariantVar === MyEnum::Variant because the lhs is data and the rhs is a function.
fn handle_item_enum(
    item_enum: ItemEnum,
    global_data: &mut GlobalData,
    current_module: &Vec<String>,
) -> JsStmt {
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
                    value: JsExpr::Object(vec![(
                        "id".to_string(),
                        Box::new(JsExpr::LitStr(variant.ident.to_string())),
                    )]),
                });
            }
        };
    }

    let mut methods = Vec::new();
    for variant in &item_enum.variants {
        let (input_names, body_stmts) = match &variant.fields {
            syn::Fields::Named(_fields_named) => {
                // for thing in fields_named.named {
                //     let name = thing.ident.unwrap();
                // }
                let stmt = JsStmt::Raw(format!(
                    r#"return {{ id: "{}", data }};"#,
                    variant.ident.to_string()
                ));
                (vec!["data".to_string()], vec![stmt])
            }
            syn::Fields::Unnamed(fields_unnamed) => {
                // const data = { id: "Baz" };
                // data.data = [text, num];
                // return data;
                let mut stmts = Vec::new();
                stmts.push(JsStmt::Raw(format!(
                    r#"const data = {{ id: "{}" }};"#,
                    variant.ident.to_string()
                )));
                let arg_names = fields_unnamed
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format!("arg_{i}"))
                    .collect::<Vec<_>>();
                stmts.push(JsStmt::Raw(format!(
                    r#"data.data = [{}];"#,
                    arg_names.join(", ")
                )));
                stmts.push(JsStmt::Raw("return data;".to_string()));
                (arg_names, stmts)
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

    let mut name = item_enum.ident.to_string();
    if let Some(dup) = global_data
        .duplicates
        .iter()
        .find(|dup| dup.name == name && &dup.original_module_path == current_module)
    {
        name = dup
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
        name,
        inputs: Vec::new(),
        static_fields,
        methods,
    })
}

fn handle_item(
    item: Item,
    is_module: bool,
    global_data: &mut GlobalData,
    current_module: Vec<String>,
    // module_object: &mut Vec<(String, Box<JsExpr>)>,
    js_stmts: &mut Vec<JsStmt>,
    // crate_path: Option<PathBuf>,
    current_file_path: &mut Option<PathBuf>,
) {
    match item {
        Item::Const(item_const) => {
            let mut name = item_const.ident.to_string();
            if let Some(dup) = global_data
                .duplicates
                .iter()
                .find(|dup| dup.name == name && dup.original_module_path == current_module)
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
                value: handle_expr(&*item_const.expr, global_data, &current_module),
            });
            js_stmts.push(local_stmt);
        }
        Item::Enum(item_enum) => {
            js_stmts.push(handle_item_enum(item_enum, global_data, &current_module));
        }
        Item::ExternCrate(_) => todo!(),
        Item::Fn(item_fn) => {
            js_stmts.push(handle_item_fn(&item_fn, global_data, &current_module));
        }
        Item::ForeignMod(_) => todo!(),
        Item::Impl(item_impl) => {
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
            let class_name = match *item_impl.self_ty {
                Type::Path(type_path) => type_path.path.segments.first().unwrap().ident.to_string(),
                _ => todo!(),
            };

            let mut impl_stmts = Vec::new();
            for impl_item in item_impl.items {
                match impl_item {
                    ImplItem::Const(impl_item_const) => {
                        // impl_item_const
                        impl_stmts.push(ImplItemTemp {
                            // class_name: impl_item_const.ident.to_string(),
                            class_name: class_name.clone(),
                            module_path: current_module.clone(),
                            item_stmt: JsStmt::ClassStatic(JsLocal {
                                public: false,
                                export: false,
                                type_: LocalType::Static,
                                lhs: LocalName::Single(impl_item_const.ident.to_string()),
                                value: handle_expr(
                                    &impl_item_const.expr,
                                    global_data,
                                    &current_module,
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
                        let body_stmts = item_impl_fn
                            .block
                            .stmts
                            .into_iter()
                            .map(|stmt| handle_stmt(&stmt, global_data, &current_module))
                            .collect::<Vec<_>>();
                        impl_stmts.push(
                            // item_impl_fn.sig.ident.to_string(),
                            ImplItemTemp {
                                class_name: class_name.clone(),
                                module_path: current_module.clone(),
                                item_stmt: JsStmt::ClassMethod(
                                    class_name.clone(),
                                    false,
                                    static_,
                                    JsFn {
                                        iife: false,
                                        public: false,
                                        export: false,
                                        is_method: true,
                                        async_: item_impl_fn.sig.asyncness.is_some(),
                                        name: camel(item_impl_fn.sig.ident),
                                        input_names,
                                        body_stmts,
                                    },
                                ),
                            },
                        )
                    }
                    ImplItem::Type(_) => todo!(),
                    ImplItem::Macro(_) => todo!(),
                    ImplItem::Verbatim(_) => todo!(),
                    _ => todo!(),
                }
            }
            if is_module {
                global_data.impl_items.extend(impl_stmts);
            } else {
                js_stmts.extend(
                    impl_stmts
                        .into_iter()
                        .map(|ImplItemTemp { item_stmt, .. }| item_stmt)
                        .collect::<Vec<_>>(),
                );
            }
        }
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

            let mut module_path = current_module.clone();
            let current_module_name = module_path.last().unwrap().clone();
            module_path.push(item_mod.ident.to_string());
            let mut module_path2 = module_path.clone();
            // TODO get rid of this
            if let Some(first) = module_path2.first() {
                if first == "crate" {
                    module_path2.remove(0);
                }
            }
            let mut file_path = global_data.crate_path.clone();
            file_path.push("src");
            if module_path2.is_empty() {
                file_path.push("main.rs");
            } else {
                let last = module_path2.last_mut().unwrap();
                last.push_str(".rs");
                file_path.extend(module_path2);
            }
            let file = if let Some(_content) = &item_mod.content {
                // TODO how does `mod bar { mod foo; }` work?
                todo!()
            } else {
                let code = fs::read_to_string(&file_path).unwrap();
                syn::parse_file(&code).unwrap()
            };

            // NOTE excluding use of attributes, only modules that are the directory parent can `mod foo`, any anywhere else we have to use `use` not `mod`.
            // In rust `mod foo` is largely redundant except for defining visibility and attributes https://stackoverflow.com/questions/32814653/why-is-there-a-mod-keyword-in-rust

            // convert from `syn` to `JsStmts`, passing the updated `current_file_path` to be used by any `mod` calls within the new module
            let mut stmts = js_stmts_from_syn_items(
                file.items,
                true,
                module_path.clone(),
                global_data,
                current_file_path,
            );

            // // Get names of pub JsStmts
            // // TODO shouldn't be using .export field as this is for importing from separate files. We don't want to add "export " to public values in a module, simply add them to the return statement of the function.
            // let defined_names = &mut global_data
            //     .modules
            //     .iter_mut()
            //     .find(|module| module.path == module_path)
            //     .unwrap()
            //     .defined_names;
            // for stmt in &stmts {
            //     match stmt {
            //         JsStmt::Class(js_class) => {
            //             defined_names.push(js_class.name.clone());
            //         }
            //         JsStmt::ClassMethod(_, _, _, _) => {}
            //         JsStmt::ClassStatic(_) => {}
            //         JsStmt::Local(js_local) => {
            //             if js_local.public {
            //                 if let LocalName::Single(name) = &js_local.lhs {
            //                     defined_names.push(name.clone())
            //                 } else {
            //                     // https://github.com/rust-lang/rfcs/issues/3290
            //                     panic!("consts do not support destructuring");
            //                 }
            //             }
            //         }
            //         JsStmt::Expr(_, _) => {}
            //         JsStmt::Import(_, _, _) => {}
            //         JsStmt::Function(js_fn) => {
            //             defined_names.push(js_fn.name.clone());
            //         }
            //         JsStmt::ScopeBlock(_) => {}
            //         JsStmt::TryBlock(_) => {}
            //         JsStmt::CatchBlock(_, _) => {}
            //         JsStmt::Raw(_) => {}
            //         JsStmt::Module(_) => {}
            //         JsStmt::Use(_) => {}
            //         JsStmt::Comment(_) => {}
            //     }
            // }

            // add return Object containing public items
            // TODO object key and value have same name so don't need to specify value
            // stmts.push(JsStmt::Raw(format!("return {{ {} }};", names.join(", "))));

            // Wrap mod up in an iffe assigned to the mod name eg
            // var myModule = (function myModule() {
            //     ...
            //     return { publicModuleItem1: publicModuleItem1, publicModuleItem2: publicModuleItem2 };
            // })();

            js_stmts.push(JsStmt::Module(JsStmtModule {
                public: match item_mod.vis {
                    Visibility::Public(_) => true,
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => false,
                },
                name: camel(item_mod.ident),
                module_path,
                stmts,
            }))

            // let mut stmts = Vec::new();
            // for thing in item_mod.content.unwrap().1 {
            //     handle_item(thing, &mut stmts);
            // }
            // js_stmts.push(JsStmt::ScopeBlock(stmts))
        }
        Item::Static(_) => todo!(),
        Item::Struct(item_struct) => {
            let mut name = item_struct.ident.to_string();
            if let Some(dup) = global_data
                .duplicates
                .iter()
                .find(|dup| dup.name == name && dup.original_module_path == current_module)
            {
                name = dup
                    .namespace
                    .iter()
                    .map(|seg| camel(seg))
                    .collect::<Vec<_>>()
                    .join("__");
            }
            let js_stmt = JsStmt::Class(JsClass {
                export: false,
                public: match item_struct.vis {
                    Visibility::Public(_) => true,
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => false,
                },
                name,
                inputs: item_struct
                    .fields
                    .into_iter()
                    .map(|field| match field.ident {
                        Some(ident) => camel(ident),
                        None => todo!(),
                    })
                    .collect::<Vec<_>>(),
                static_fields: Vec::new(),
                methods: Vec::new(),
            });
            js_stmts.push(js_stmt);
        }
        Item::Trait(_) => js_stmts.push(JsStmt::Expr(JsExpr::Vanish, false)),
        Item::TraitAlias(_) => todo!(),
        Item::Type(_) => todo!(),
        Item::Union(_) => todo!(),
        Item::Use(_) => {}
        Item::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}

pub fn from_file_with_main(code: &str) -> Vec<JsStmt> {
    let mut js_stmts = from_file(code, true);
    js_stmts.push(JsStmt::Expr(
        JsExpr::FnCall(Box::new(JsExpr::Path(vec!["main".to_string()])), Vec::new()),
        true,
    ));
    js_stmts
}

pub fn stmts_with_main(mut stmts: Vec<JsStmt>) -> Vec<JsStmt> {
    stmts.push(JsStmt::Expr(
        JsExpr::FnCall(Box::new(JsExpr::Path(vec!["main".to_string()])), Vec::new()),
        true,
    ));
    stmts
}

/// Converts a Vec<syn::Item> to Vec<JsStmt> and moves method impls into their class
///
/// all users (eg crate, fn, file) want to group classes, but only crates want to populate boilerplate
pub fn js_stmts_from_syn_items(
    items: Vec<Item>,
    // Need to know whether to return a module Object or just a vec of stmts
    is_module: bool,
    // Need to keep of which module we are currently in, for constructing the boilerplate
    current_module: Vec<String>,
    global_data: &mut GlobalData,
    // crate_path: Option<PathBuf>,
    current_file_path: &mut Option<PathBuf>,
) -> Vec<JsStmt> {
    let mut js_stmts = Vec::new();
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

    // Also, impls can be inside lower *scopes* (not modules) eg inside functions (and the functions don't even need to be run)
    // fn main() {
    //     struct Cool {}
    //     fn inner() {
    //         impl Cool {
    //             fn whatever(&self) {
    //                 dbg!("hi");
    //             }
    //         }
    //     }
    //     let cool = Cool {};
    //     cool.whatever();
    // }
    // let mut module_object = Vec::new();
    for item in items {
        handle_item(
            item,
            is_module,
            global_data,
            current_module.clone(),
            // &mut module_object,
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
}

#[derive(Debug, Clone)]
struct ImplItemTemp {
    /// snake case
    class_name: String,
    /// snake case
    module_path: Vec<String>,
    item_stmt: JsStmt,
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
}

#[derive(Debug, Clone)]
struct Duplicate {
    namespace: Vec<String>,
    module_path: Vec<String>,
    name: String,
    original_module_path: Vec<String>,
}

#[derive(Debug, Clone)]
struct GlobalData {
    crate_path: PathBuf,
    modules: Vec<ModuleData>,
    rust_prelude_types: RustPreludeTypes,
    impl_items: Vec<ImplItemTemp>,
    duplicates: Vec<Duplicate>,
}
impl GlobalData {
    fn new(crate_path: PathBuf, duplicates: Vec<Duplicate>) -> GlobalData {
        GlobalData {
            crate_path,
            modules: Vec::new(),
            rust_prelude_types: RustPreludeTypes::default(),
            impl_items: Vec::new(),
            duplicates,
        }
    }
}

struct Module {
    module_path: Vec<String>,
    stmts: Vec<JsStmt>,
}

/// Match impl items to the classes in a `JsStmtModule`'s stmts and update the classes, recursively doing the same thing for any sub modules
fn update_classes(js_stmt_module: &mut JsStmtModule, impl_items: Vec<ImplItemTemp>) {
    for stmt in js_stmt_module.stmts.iter_mut() {
        match stmt {
            JsStmt::Class(js_class) => {
                for impl_item in impl_items.clone() {
                    if impl_item.module_path == js_stmt_module.module_path
                        && js_class.name == impl_item.class_name
                    {
                        match impl_item.item_stmt {
                            JsStmt::ClassStatic(js_local) => {
                                js_class.static_fields.push(js_local);
                            }
                            JsStmt::ClassMethod(name, private, static_, js_fn) => {
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
            JsStmt::Module(js_stmt_module) => update_classes(js_stmt_module, impl_items.clone()),
            _ => {}
        }
    }
}

pub fn from_crate(crate_path: PathBuf, with_rust_types: bool) -> Vec<JsStmt> {
    // dbg!(&main_path);
    let code = fs::read_to_string(crate_path.join("src").join("main.rs")).unwrap();
    let file = syn::parse_file(&code).unwrap();
    // let mut current_file_path = vec!["main.rs".to_string()];

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
        crate_path: &PathBuf,
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
                    });

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
                    if let Some(_content) = &item_mod.content {
                        // TODO how does `mod bar { mod foo; }` work?
                        todo!()
                    } else {
                        let code = fs::read_to_string(&file_path).unwrap();
                        let file = syn::parse_file(&code).unwrap();
                        extract_data(&file.items, crate_path, module_path, names, modules);
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
    });
    let mut get_names_module_path = vec!["crate".to_string()];
    // let mut get_names_crate_path = crate_path.join("src/main.rs");
    let mut get_names_crate_path = crate_path.clone();
    extract_data(
        &file.items,
        &get_names_crate_path,
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
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);
    update_dup_names(&mut duplicates);

    for dup in duplicates.iter_mut() {
        dup.namespace.push(dup.name.clone());
    }

    // Resolve use stmts
    // clone modules so we can use it to lookup up data while mutating the actual modules
    let immutable_modules = modules.clone();
    for module in &mut modules {
        // Firstly We want to resolve all items that are available at the top level of the module, but not acutally defined in the module ie all use statements. Whether they are `pub` or not is irrelevant since that is only relevant to other modules `use`ing from this module
        // We are only focussing on resolving `use` statements. Something like `mod foo; foo::bar::baz();` will get resolved during the translate stage as there is nothing we can precalculate at this point (from this module, in `foo` we can resolve the `use` stmts (ie what we are doing here) since `bar` might come from `pub use bar::baz;`)
        for (item_name, item_path) in module
            .pub_use_mappings
            .iter()
            .chain(module.private_use_mappings.iter())
        {
            // get submodule we are `use`ing from (need to compare on full path because module names are not unique)
            if let Some(submodule) = immutable_modules.iter().find(|submodule| {
                let mut submodule_path = module.path.clone();
                submodule_path.push(item_path.first().unwrap().clone());
                submodule.path == submodule_path
            }) {
                // Now we are looking in the sub module
                // we need to account for the fact that the item might be `pub use ...`'d from the sub module OR `pub mod ...`'d

                // check if we are `use`ing an item *defined* in that module
                if submodule
                    .pub_definitions
                    .iter()
                    .any(|pub_name| pub_name == item_name)
                {
                    // We found the origin of the item being used, so we can record the full path of the item
                    module
                        .resolved_mappings
                        .push((item_name.clone(), submodule.path.clone()))
                    // item is not defined in this sub module, so lets check if there is a `pub use ...` to a second submodule
                } else if let Some(use_mapping) = submodule
                    .pub_use_mappings
                    .iter()
                    .find(|(sub_item_name, _)| sub_item_name == item_path.first().unwrap())
                {
                    // again/recursive: check if we are `use`ing from a `mod`/sub module
                    if let Some(sub_module2) = immutable_modules
                        .iter()
                        .find(|sub_module2| &sub_module2.name == use_mapping.1.first().unwrap())
                    {
                        // again/recursive: check if we are `use`ing an item defined in that module
                        if sub_module2
                            .pub_definitions
                            .iter()
                            .any(|pub_defined_name2| pub_defined_name2 == item_name)
                        {
                            // again/recursive: We found the origin of the item being used, so we can record the full path of the item
                            module
                                .resolved_mappings
                                .push((item_name.clone(), submodule.path.clone()))
                        } else {
                            // again/recursive: item is not defined in this sub module, so there must be a `pub use ...` to another module
                            // TODO refactor to make it properly recursive
                            todo!()
                        }
                    } else {
                        // if we are not `use`ing from a module, we must be using from a module that has already been used, or from another crate, neither of which are supported currently
                        todo!()
                    }
                    // There is no `pub use` which exports our path name, so check for pub mod instead
                } else if let Some(submodule2_name) = submodule
                    .pub_submodules
                    .iter()
                    // First first item in item_path was the name the current submodule, now we are looking for a module `pub mod ...` from the current module which would be the second item in the path
                    .find(|sub_module_name| sub_module_name == &item_path.get(1).unwrap())
                {
                    // We matched a sub module name, now lets get the acutal second sub module
                    let sub_module2 = immutable_modules
                        .iter()
                        .find(|sub_module2| &sub_module2.name == submodule2_name)
                        .unwrap();

                    // Now we again need to check if the item we are looking for is defined in this module, or there is another `pub use ...` or `pub mod ...`
                    if sub_module2.pub_definitions.contains(item_name) {
                        module
                            .resolved_mappings
                            .push((item_name.clone(), sub_module2.path.clone()))
                    } else {
                        todo!("deeper pub use/mod nesting")
                    }
                } else {
                    panic!("no pub use or pub mod found");
                }
            } else {
                // if we are not `use`ing from a module, we must be using from a module that has already been used, or from another crate, neither of which are supported currently
                todo!()
            }
        }
    }

    dbg!(&modules);

    let mut js_stmts = Vec::new();

    // let mut module_names = Vec::new();

    // dbg!(&modules);
    let mut global_data = GlobalData::new(crate_path.clone(), duplicates.clone());
    global_data.modules = modules;

    let crate_stmts = js_stmts_from_syn_items(
        file.items,
        true,
        vec!["crate".to_string()],
        &mut global_data,
        &mut Some(crate_path.join("src").join("main.rs")),
    );

    let mut crate_module = JsStmtModule {
        public: true,
        name: "crate".to_string(),
        module_path: vec!["crate".to_string()],
        stmts: crate_stmts,
    };
    // js_stmts.extend();

    update_classes(&mut crate_module, global_data.impl_items);

    // resolve paths to get canonical path to item
    // Update paths to use namespaced names and account for `use` statements
    // We want to find the module where the item is actually defined, not where it is mod, use, pub from other modules
    // Solution:
    // x First find all the items defined in each module
    // Then for each module, add maps of the names of the items that are mod/use'd to the use path
    // go through each module and reconcile each item with the use mapping, and that with the module path where the item is actually defined

    // Maybe do a minimal pass before the main parsing, where we only look at ::Mod and ::Use to build up module data. Then, we can resolve path names directly in the parsing pass, rather than having to iterate through everything in this second pass? Remeber `use` can be anywhere so trading iterating through everything JsStmts for syn is an improvement. Plus it just makes a lot more sense to get that data from the syn types, rather than

    fn resolve_names_jsexpr(js_expr: &mut JsExpr) {
        match js_expr {
            JsExpr::Array(_) => todo!(),
            JsExpr::ArrowFn(_, _, _, _) => todo!(),
            JsExpr::Assignment(_, _) => todo!(),
            JsExpr::Binary(lhs, _, rhs) => {}
            JsExpr::Blank => todo!(),
            JsExpr::Block(_) => todo!(),
            JsExpr::Break => todo!(),
            JsExpr::Declaration(_, _, _) => todo!(),
            JsExpr::Null => todo!(),
            JsExpr::LitInt(_) => todo!(),
            JsExpr::LitStr(_) => todo!(),
            JsExpr::LitBool(_) => todo!(),
            JsExpr::Object(_) => todo!(),
            JsExpr::ObjectForModule(_) => todo!(),
            JsExpr::Return(_) => todo!(),
            JsExpr::Vanish => todo!(),
            JsExpr::Field(_, _) => todo!(),
            JsExpr::Fn(_) => todo!(),
            JsExpr::FnCall(_, _) => todo!(),
            JsExpr::If(_, _, _, _, _) => todo!(),
            JsExpr::Paren(_) => todo!(),
            JsExpr::Not(_) => todo!(),
            JsExpr::Minus(_) => todo!(),
            JsExpr::Await(_) => todo!(),
            JsExpr::Index(_, _) => todo!(),
            JsExpr::Var(_) => todo!(),
            JsExpr::Path(path) => {
                path.clear();
                path.push("cool".to_string());
            }
            JsExpr::New(_, _) => todo!(),
            JsExpr::MethodCall(_, _, _) => todo!(),
            JsExpr::While(_, _) => todo!(),
            JsExpr::ForLoop(_, _, _) => todo!(),
        }
    }
    fn resolve_names(
        this_module: &mut Vec<String>,
        stmts: &mut Vec<JsStmt>,
        modules: &Vec<ModuleData>,
    ) {
        for stmt in stmts {
            match stmt {
                JsStmt::Class(js_class) => {
                    for static_field in &mut js_class.static_fields {
                        resolve_names_jsexpr(&mut static_field.value);
                    }
                    for method in &mut js_class.methods {
                        resolve_names(this_module, &mut method.3.body_stmts, modules);
                    }
                }
                JsStmt::ClassMethod(_, _, _, _) => todo!(),
                JsStmt::ClassStatic(_) => todo!(),
                JsStmt::Local(js_local) => {
                    resolve_names_jsexpr(&mut js_local.value);
                }
                JsStmt::Expr(js_expr, _) => resolve_names_jsexpr(js_expr),
                JsStmt::Import(_, _, _) => todo!(),
                JsStmt::Function(js_fn) => {
                    resolve_names(this_module, &mut js_fn.body_stmts, modules);
                }
                JsStmt::ScopeBlock(block_stmts) => resolve_names(this_module, block_stmts, modules),
                JsStmt::TryBlock(block_stmts) => resolve_names(this_module, block_stmts, modules),
                JsStmt::CatchBlock(_, block_stmts) => {
                    resolve_names(this_module, block_stmts, modules)
                }
                JsStmt::Raw(_) => todo!(),
                JsStmt::Module(js_stmt_module) => {
                    this_module.push(js_stmt_module.name.clone());
                    resolve_names(this_module, &mut js_stmt_module.stmts, modules);
                    this_module.pop();
                }
                JsStmt::Use(_) => todo!(),
                JsStmt::Comment(_) => todo!(),
            }
        }
    }
    let mut current_module = vec!["crate".to_string()];
    // resolve_names(
    //     &mut current_module,
    //     &mut crate_module.stmts,
    //     &global_data.modules,
    // );

    // Remember that use might only be `use`ing a module, and then completing the path to the actual item in the code. So the final step of reconciliation will always need make use of the actual paths/items in the code
    // dbg!(global_data.modules);

    if with_rust_types {
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
        js_stmts.push(JsStmt::Class(JsClass {
            export: false,
            public: false,
            name: "Vec".to_string(),
            inputs: Vec::new(),
            static_fields: Vec::new(),
            methods,
        }));
    }

    vec![JsStmt::Module(crate_module)]
}

pub fn from_module(code: &str, with_vec: bool) -> Vec<JsStmt> {
    let item_mod = syn::parse_str::<ItemMod>(code).unwrap();
    let items = item_mod.content.unwrap().1;
    let mut current_module = Vec::new();
    let mut global_data = GlobalData::new(PathBuf::new(), Vec::new());
    js_stmts_from_syn_items(items, true, current_module, &mut global_data, &mut None)
}

pub fn from_file(code: &str, with_vec: bool) -> Vec<JsStmt> {
    let file = syn::parse_file(code).unwrap();
    // let mut current_file_path = Vec::new();
    let mut current_module = Vec::new();
    let mut global_data = GlobalData::new(PathBuf::new(), Vec::new());
    let stmts = js_stmts_from_syn_items(
        file.items,
        true,
        current_module,
        &mut global_data,
        &mut None,
    );
    let mut crate_module = JsStmtModule {
        public: false,
        name: "crate".to_string(),
        module_path: Vec::new(),
        stmts,
    };

    update_classes(&mut crate_module, global_data.impl_items);
    crate_module.stmts
}

pub fn from_fn(code: &str) -> Vec<JsStmt> {
    let item_fn = syn::parse_str::<ItemFn>(code).unwrap();

    let mut js_stmts = Vec::new();
    for stmt in &item_fn.block.stmts {
        let js_stmt = handle_stmt(
            stmt,
            &mut GlobalData::new(PathBuf::new(), Vec::new()),
            &Vec::new(),
        );
        js_stmts.push(js_stmt);
    }
    js_stmts
}

pub fn from_block(code: &str) -> Vec<JsStmt> {
    let expr_block = syn::parse_str::<ExprBlock>(code).unwrap();

    let mut js_stmts = Vec::new();
    for stmt in &expr_block.block.stmts {
        let js_stmt = handle_stmt(
            stmt,
            &mut GlobalData::new(PathBuf::new(), Vec::new()),
            &Vec::new(),
        );
        js_stmts.push(js_stmt);
    }
    js_stmts
}

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
        }
    }
    fn from_binop(binop: BinOp) -> JsOp {
        match binop {
            BinOp::Add(_) => JsOp::Add,
            BinOp::Sub(_) => JsOp::Sub,
            BinOp::Mul(_) => todo!(),
            BinOp::Div(_) => todo!(),
            BinOp::Rem(_) => todo!(),
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
        }
    }
}

#[derive(Clone, Debug)]
pub enum JsExpr {
    Array(Vec<JsExpr>),
    /// (async, block, inputs, body)
    ArrowFn(bool, bool, Vec<String>, Vec<JsStmt>),
    Assignment(Box<JsExpr>, Box<JsExpr>),
    Binary(Box<JsExpr>, JsOp, Box<JsExpr>),
    /// Will only make itself disappear
    Blank,
    Block(Vec<JsStmt>),
    Break,
    /// (const/let/var, left, right)
    /// use const for immutatble, let for mutable, var for shadowing
    Declaration(bool, String, Box<JsExpr>),
    Null,
    LitInt(i32),
    LitStr(String),
    LitBool(bool),
    Object(Vec<(String, Box<JsExpr>)>),
    ObjectForModule(Vec<JsStmt>),
    Return(Box<JsExpr>),
    /// Will make the entire statement disappear no matter where it is nested?
    Vanish,
    /// (base var name, field name)
    Field(Box<JsExpr>, String),
    Fn(JsFn),
    /// (name, args)
    FnCall(Box<JsExpr>, Vec<JsExpr>),
    /// `if else` statements are achieved by nesting an additional if statement as the fail arg.
    /// A problem is that Some assignment triggers a `var = x;`, however we also need to know whether we are doing assignment in nested If's (if else) but without adding a new var declaration. need to add another flag just to say when we need to declare the var
    ///
    /// (assignment, declare_var, condition, succeed, fail)
    If(
        Option<LocalName>,
        bool,
        Box<JsExpr>,
        Vec<JsStmt>,
        /// For some reason syn has an expr as the else branch, rather than the typical iter of statements - because the expr might be another if expr, not always a block
        Option<Box<JsExpr>>,
    ),
    Paren(Box<JsExpr>),
    Not(Box<JsExpr>),
    Minus(Box<JsExpr>),
    Await(Box<JsExpr>),
    Index(Box<JsExpr>, Box<JsExpr>),
    Var(String),
    /// like obj::inner::mynumber -> obj.inner.mynumber;
    Path(Vec<String>),
    New(Vec<String>, Vec<JsExpr>),
    /// (receiver, method name, method args)
    /// TODO assumes receiver is single var
    MethodCall(Box<JsExpr>, String, Vec<JsExpr>),
    // Class(JsClass),
    While(Box<JsExpr>, Vec<JsStmt>),
    /// (pat, expr, block)
    ForLoop(String, Box<JsExpr>, Vec<JsStmt>),
}

// Make a struct called If with these fields so I can define js_string() on the struct and not have this fn
fn if_expr_to_string(
    assignment: &Option<LocalName>,
    declare_var: &bool,
    cond: &Box<JsExpr>,
    succeed: &Vec<JsStmt>,
    // For some reason syn has an expr as the else branch, rather than the typical iter of statements - because the expr might be another if expr, not always a block
    else_: &Option<Box<JsExpr>>,
) -> String {
    let else_ = if let Some(else_) = else_ {
        match &**else_ {
            JsExpr::If(_, _, cond, succeed, fail) => format!(
                " else {}",
                JsExpr::If(
                    assignment.clone(),
                    false,
                    cond.clone(),
                    succeed.clone(),
                    fail.clone()
                )
                .js_string()
            ),
            _ => {
                let thing = match &**else_ {
                    JsExpr::Block(stmts) => stmts
                        .iter()
                        .enumerate()
                        .map(|(i, stmt)| {
                            if i == stmts.len() - 1 {
                                if let Some(assignment) = assignment {
                                    format!("{} = {};", assignment.js_string(), stmt.js_string())
                                } else {
                                    stmt.js_string()
                                }
                            } else {
                                stmt.js_string()
                            }
                        })
                        .collect::<Vec<_>>()
                        .join("\n"),
                    _ => {
                        if let Some(assignment) = assignment {
                            format!("{} = {};", assignment.js_string(), else_.js_string())
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

                        format!("{} = {};", assignment.js_string(), stmt.js_string())
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
impl JsExpr {
    fn js_string(&self) -> String {
        match self {
            JsExpr::LitInt(int) => int.to_string(),
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
                // let body = if body.len() == 1 {
                //     // concise body
                //     match body.get(0).unwrap() {
                //         JsStmt::Local(js_local) => js_local.js_string(),
                //         // TODO single objects returned by concise body must be wrapped in parenthesis
                //         JsStmt::Expr(js_expr, closing_semi) => {
                //             if *closing_semi {
                //                 match js_expr {
                //                     JsExpr::If(_, _, _, _, _) => panic!(),
                //                     _ => js_expr.js_string(),
                //                 }
                //             } else {
                //                 match js_expr {
                //                     JsExpr::If(_, _, _, _, _) => {
                //                         format!("{{\n{}\n}}", js_expr.js_string())
                //                     }
                //                     _ => js_expr.js_string(),
                //                 }
                //             }
                //         }
                //         JsStmt::Import(_, _, _) => todo!(),
                //         JsStmt::Function(_) => todo!(),
                //         JsStmt::Class(_) => todo!(),
                //         JsStmt::ClassMethod(_, _, _, _) => todo!(),
                //         JsStmt::ClassStatic(_) => todo!(),
                //         JsStmt::Raw(_) => todo!(),
                //         JsStmt::ScopeBlock(_) => todo!(),
                //         JsStmt::CatchBlock(_, _) => todo!(),
                //         JsStmt::TryBlock(_) => todo!(),
                //     }
                // } else {
                //     body.iter()
                //         .enumerate()
                //         .map(|(i, stmt)| handle_js_body_stmts(i, stmt, body.len()))
                //         .collect::<Vec<_>>()
                //         .join("\n")
                // };
                let body = if *block {
                    body.iter()
                        .enumerate()
                        .map(|(i, stmt)| handle_fn_body_stmts(i, stmt, body.len()))
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
            JsExpr::If(assignment, declare_var, cond, succeed, fail) => {
                if_expr_to_string(assignment, declare_var, cond, succeed, fail)
            }
            JsExpr::Declaration(_, name, expr) => format!("var {name} = {}", expr.js_string()),
            JsExpr::Break => "break".to_string(),
            JsExpr::Not(expr) => format!("!{}", expr.js_string()),
            JsExpr::Block(stmts) => stmts
                .iter()
                .map(|stmt| stmt.js_string())
                .collect::<Vec<_>>()
                .join("\n"),
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
                let js_stmt_module = JsStmtModule {
                    public: false,
                    name: "whatever".to_string(),
                    module_path: vec![],
                    stmts: js_stmts.clone(),
                };
                // dbg!(&js_stmt_module);
                js_stmt_module.js_string()
            }
        }
    }
}

// ::new()/Constructor must assign all fields of class
#[derive(Clone, Debug)]
pub struct JsClass {
    /// None if class is not defined at module level
    // module_path: Option<Vec<String>>,
    public: bool,
    export: bool,
    name: String,
    /// we are assuming input names is equivalent to field names
    inputs: Vec<String>,
    /// (class name, private, static, JsFn)  
    // static_fields: Vec<(String, JsLocal)>,
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

#[derive(Clone, Debug)]
struct DestructureObject(Vec<DestructureValue>);
impl DestructureObject {
    fn js_string(&self) -> String {
        format!(
            "{{ {} }}",
            self.0
                .iter()
                .map(|name| match name {
                    DestructureValue::KeyName(key) => key.clone(),
                    DestructureValue::Rename(key, new_name) => format!("{key}: {new_name}"),
                    DestructureValue::Nesting(key, destructure_object) =>
                        format!("{key}: {}", destructure_object.js_string()),
                })
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Clone, Debug)]
pub enum LocalName {
    Single(String),
    DestructureObject(DestructureObject),
    DestructureArray(Vec<String>),
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
                    .map(|name| name.clone())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct JsLocal {
    /// None if not a const is not defined at module level
    // module_path: Option<Vec<String>>,
    public: bool,
    export: bool,
    type_: LocalType,
    lhs: LocalName,
    // names: Vec<String>,
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
    /// None if not a fn defined at module level
    // module_path: Option<Vec<String>>,
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
fn handle_fn_body_stmts(i: usize, stmt: &JsStmt, len: usize) -> String {
    match stmt {
        JsStmt::Local(js_local) => js_local.js_string(),
        JsStmt::Expr(js_expr, semi) => match js_expr {
            JsExpr::If(assignment, _, _, _, _) => {
                // TODO wrongly assuming that all single if expr bodys should be returned
                if i == len - 1 {
                    if let Some(assignment) = assignment {
                        format!(
                            "{}\nreturn {};",
                            js_expr.js_string(),
                            assignment.js_string()
                        )
                    } else {
                        todo!()
                    }
                } else {
                    js_expr.js_string()
                }
            }
            JsExpr::Block(_) => js_expr.js_string(),
            JsExpr::While(_, _) => js_expr.js_string(),
            JsExpr::ForLoop(_, _, _) => js_expr.js_string(),
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
        JsStmt::Module(_) => stmt.js_string(),
        JsStmt::Comment(_) => stmt.js_string(),
        JsStmt::Use(_) => String::new(),
    }
}
impl JsFn {
    fn js_string(&self) -> String {
        // dbg!(self);
        // TODO private fields and methods should be prepended with `#` like `#private_method() {}` but this would require also prepending all callsites of the the field or method, which requires more sophisticated AST analysis than we currently want to do.
        let body_stmts = self
            .body_stmts
            .iter()
            .enumerate()
            .map(|(i, stmt)| handle_fn_body_stmts(i, stmt, self.body_stmts.len()))
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
struct JsStmtModule {
    public: bool,
    /// camelCase JS name
    name: String,
    /// snake_case Rust path
    module_path: Vec<String>,
    stmts: Vec<JsStmt>,
}
impl JsStmtModule {
    fn js_string(&self) -> String {
        let items = self
            .stmts
            .iter()
            .map(|stmt| {
                // let name = match stmt {
                //     JsStmt::Class(js_class) => js_class.name.clone(),
                //     JsStmt::ClassMethod(_, _, _, _) => todo!("cannot be js module item"),
                //     JsStmt::ClassStatic(_) => todo!("cannot be js module item"),
                //     JsStmt::Local(js_local) => {
                //         if let LocalName::Single(name) = &js_local.lhs {
                //             // only want the `js_local`'s value, not the whole `var name = value;`
                //             return format!("{name}: {}", js_local.value.js_string());
                //         } else {
                //             // https://github.com/rust-lang/rfcs/issues/3290
                //             panic!("consts do not support destructuring");
                //         }
                //     }
                //     JsStmt::Expr(js_expr, _) => {
                //         dbg!(js_expr);
                //         todo!("cannot be js module item")
                //     }
                //     JsStmt::Import(_, _, _) => todo!("cannot be js module item"),
                //     JsStmt::Function(js_fn) => js_fn.name.clone(),
                //     JsStmt::ScopeBlock(_) => todo!("cannot be js module item"),
                //     JsStmt::TryBlock(_) => todo!("cannot be js module item"),
                //     JsStmt::CatchBlock(_, _) => todo!("cannot be js module item"),
                //     JsStmt::Raw(_) => todo!("cannot be js module item"),
                //     JsStmt::Module(js_stmt_module) => js_stmt_module.name.clone(),
                //     // TODO support allowing comments as module items so they can appear before the item in the object
                //     JsStmt::Comment(_) => todo!("cannot be js module item"),
                // };
                // let value = stmt.js_string();
                // format!("{name}: {value}")
                stmt.js_string()
            })
            .collect::<Vec<_>>()
            .join("\n\n");
        format!(
            "// {}::{}\n{}",
            self.module_path.join("::"),
            self.name,
            items
        )
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
    /// Unlike the other variants this has meaning for the parsing/transpiling, and isn't just a representation of what to write like the other variants
    Module(JsStmtModule),
    /// Unlike the other variants this *only* has meaning for the parsing/transpiling, and isn't output (except maybe comments for debugging?)
    ///
    /// (path, item name)
    Use(Vec<(Vec<String>, String)>),
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
            JsStmt::Module(js_stmt_module) => js_stmt_module.public,
            JsStmt::Comment(_) => todo!(),
            JsStmt::Use(_) => todo!(),
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
                    "class {} {{\nconstructor({}) {{\n{}\n}}\n{}\n{}}}",
                    js_class.name,
                    js_class.inputs.join(", "),
                    js_class
                        .inputs
                        .iter()
                        .map(|input| format!("this.{input} = {input};"))
                        .collect::<Vec<_>>()
                        .join(" "),
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
            JsStmt::Module(js_stmt_module) => js_stmt_module.js_string(),
            JsStmt::Comment(text) => format!("// {text}"),
            JsStmt::Use(_) => "".to_string(),
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
                                JsStmt::Expr(
                                    JsExpr::If(
                                        Some(LocalName::Single("ifTempAssignment".to_string())),
                                        true,
                                        Box::new(handle_expr(
                                            &*expr_if.cond,
                                            global_data,
                                            current_module,
                                        )),
                                        expr_if
                                            .then_branch
                                            .stmts
                                            .iter()
                                            .map(|stmt| {
                                                handle_stmt(stmt, global_data, current_module)
                                            })
                                            .collect::<Vec<_>>(),
                                        expr_if.else_branch.as_ref().map(|(_, expr)| {
                                            Box::new(handle_expr(
                                                &*expr,
                                                global_data,
                                                current_module,
                                            ))
                                        }),
                                    ),
                                    false,
                                )
                            }
                        }
                        Expr::Match(_) => todo!(),
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

fn handle_expr(expr: &Expr, global_data: &mut GlobalData, current_module: &Vec<String>) -> JsExpr {
    match expr {
        Expr::Array(_) => todo!(),
        Expr::Assign(expr_assign) => JsExpr::Assignment(
            Box::new(handle_expr(&*expr_assign.left, global_data, current_module)),
            Box::new(handle_expr(
                &*expr_assign.right,
                global_data,
                current_module,
            )),
        ),
        Expr::Async(_) => todo!(),
        Expr::Await(expr_await) => JsExpr::Await(Box::new(handle_expr(
            &*expr_await.base,
            global_data,
            current_module,
        ))),
        Expr::Binary(expr_binary) => JsExpr::Binary(
            Box::new(handle_expr(&*expr_binary.left, global_data, current_module)),
            JsOp::from_binop(expr_binary.op),
            Box::new(handle_expr(
                &*expr_binary.right,
                global_data,
                current_module,
            )),
        ),
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
            let args = expr_call
                .args
                .iter()
                .map(|arg| handle_expr(arg, global_data, current_module))
                .collect::<Vec<_>>();
            match &*expr_call.func {
                Expr::Path(expr_path)
                    if expr_path.path.segments.last().unwrap().ident.to_string() == "fetch2" =>
                {
                    // TODO improve this code
                    JsExpr::FnCall(Box::new(JsExpr::Path(vec!["fetch".to_string()])), args)
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
                Expr::Path(expr_path)
                    if expr_path.path.segments.last().unwrap().ident.to_string() == "stringify" =>
                {
                    JsExpr::FnCall(
                        Box::new(JsExpr::Path(vec![
                            "JSON".to_string(),
                            "stringify".to_string(),
                        ])),
                        args,
                    )
                }
                Expr::Path(expr_path)
                    if expr_path.path.segments.len() == 2
                        && expr_path.path.segments[0].ident.to_string() == "Json"
                        && expr_path.path.segments[1].ident.to_string() == "parse" =>
                {
                    JsExpr::FnCall(
                        Box::new(JsExpr::Path(vec!["JSON".to_string(), "parse".to_string()])),
                        args,
                    )
                }
                Expr::Path(expr_path)
                    if expr_path.path.segments.len() == 2
                        && expr_path.path.segments[0].ident.to_string() == "Date"
                        && expr_path.path.segments[1].ident.to_string() == "from_iso_string" =>
                {
                    JsExpr::New(vec!["Date".to_string()], args)
                }
                Expr::Path(expr_path)
                    if expr_path.path.segments.len() == 2
                        && expr_path.path.segments[0].ident.to_string() == "Document"
                        && expr_path.path.segments[1].ident.to_string()
                            == "query_selector_body" =>
                {
                    JsExpr::FnCall(
                        Box::new(JsExpr::Path(vec![
                            "document".to_string(),
                            "querySelector".to_string(),
                        ])),
                        vec![JsExpr::LitStr("body".to_string())],
                    )
                }
                Expr::Path(expr_path)
                    if expr_path.path.segments.len() == 2
                        && expr_path.path.segments[0].ident.to_string() == "Document"
                        && expr_path.path.segments[1].ident.to_string() == "create_element_div" =>
                {
                    JsExpr::FnCall(
                        Box::new(JsExpr::Path(vec![
                            "document".to_string(),
                            "createElement".to_string(),
                        ])),
                        vec![JsExpr::LitStr("div".to_string())],
                    )
                }
                Expr::Path(expr_path)
                    if expr_path.path.segments.len() == 1
                        && expr_path.path.segments[0].ident.to_string() == "Some" =>
                {
                    args.into_iter().next().unwrap()
                }
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
        Expr::Field(expr_field) => JsExpr::Field(
            Box::new(handle_expr(&*expr_field.base, global_data, current_module)),
            match &expr_field.member {
                Member::Named(ident) => camel(ident),
                Member::Unnamed(_) => todo!(),
            },
        ),
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
        Expr::If(expr_if) => JsExpr::If(
            None,
            false,
            Box::new(handle_expr(&*expr_if.cond, global_data, current_module)),
            expr_if
                .then_branch
                .stmts
                .iter()
                .map(|stmt| handle_stmt(stmt, global_data, current_module))
                .collect::<Vec<_>>(),
            expr_if
                .else_branch
                .as_ref()
                .map(|(_, expr)| Box::new(handle_expr(&*expr, global_data, current_module))),
        ),
        Expr::Index(expr_index) => JsExpr::Index(
            Box::new(handle_expr(&*expr_index.expr, global_data, current_module)),
            Box::new(handle_expr(&*expr_index.index, global_data, current_module)),
        ),
        Expr::Infer(_) => todo!(),
        Expr::Let(expr_let) => {
            dbg!(expr_let);
            todo!()
        }
        Expr::Lit(expr_lit) => match &expr_lit.lit {
            Lit::Str(lit_str) => JsExpr::LitStr(lit_str.value()),
            Lit::ByteStr(_) => todo!(),
            Lit::Byte(_) => todo!(),
            Lit::Char(_) => todo!(),
            Lit::Int(lit_int) => JsExpr::LitInt(lit_int.base10_parse::<i32>().unwrap()),
            Lit::Float(_) => todo!(),
            Lit::Bool(lit_bool) => JsExpr::LitBool(lit_bool.value),
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
            let path_segs = expr_macro
                .mac
                .path
                .segments
                .iter()
                .map(|seg| seg.ident.to_string())
                .collect::<Vec<_>>();
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
            JsExpr::Vanish
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
            if method_name == "is_some" {
                return JsExpr::Binary(Box::new(receiver), JsOp::NotEq, Box::new(JsExpr::Null));
            }
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
            let segs = expr_path
                .path
                .segments
                .iter()
                .map(|seg| {
                    let mut var_name = seg.ident.to_string();
                    if var_name == "self" {
                        var_name = "this".to_string();
                    }
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
                    case_convert(var_name)
                })
                .collect::<Vec<_>>();
            if segs.len() == 1 {
                if segs[0] == "None" {
                    return JsExpr::Null;
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
                JsExpr::Return(Box::new(handle_expr(&*expr, global_data, current_module)))
            } else {
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
                .map(|elem| match elem {
                    Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
                    _ => todo!(),
                })
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
        JsExpr::LitStr("this shouldn't exist".to_string()),
        |acc, arm| {
            let (mut rhs, mut body_data_destructure) =
                handle_match_pat(&arm.pat, expr_match, global_data, current_module);

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

            JsExpr::If(
                is_returned.then_some(LocalName::Single("ifTempAssignment".to_string())),
                is_returned,
                Box::new(JsExpr::Binary(
                    Box::new(JsExpr::Field(
                        Box::new(handle_expr(&*expr_match.expr, global_data, current_module)),
                        "id".to_string(),
                    )),
                    JsOp::Eq,
                    Box::new(JsExpr::Path(rhs)),
                )),
                body,
                // TODO
                Some(Box::new(acc)),
            )
        },
    );
    // for arm in &expr_match.arms {
    //     dbg!(arm);
    // }
    // todo!()
    if_expr
}

pub mod web {
    use std::{any::Any, future::Future, ops::Index};

    #[derive(Debug, Default)]
    pub struct RegExp {}
    impl RegExp {
        pub fn new(_pattern: &str, _global: &str) -> RegExp {
            RegExp::default()
        }
    }

    pub trait JsPattern {}
    impl JsPattern for RegExp {}
    impl JsPattern for &str {}
    pub trait Replace {
        fn js_replace(&self, _pattern: impl JsPattern, _replace_with: &str) -> Self
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl Replace for &str {}

    #[derive(Clone, Copy, Debug, Default)]
    pub struct JsArray {
        pub length: i32,
    }
    impl JsArray {
        pub fn slice(self, _index: i32) -> JsArray {
            JsArray::default()
        }
    }
    impl Iterator for JsArray {
        type Item = &'static str;

        fn next(&mut self) -> Option<Self::Item> {
            None
        }
    }

    pub trait Slice {
        fn slice1(self, _index: i32) -> Self
        where
            Self: Sized,
        {
            todo!()
        }
        fn slice2(self, _index1: i32, _index2: i32) -> Self
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl Slice for &str {}

    pub trait Concat {
        fn concat(self, _arg: &str) -> &str
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl Concat for &str {}

    pub trait Includes {
        fn includes(&self, _pattern: &str) -> bool
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl Includes for &str {}

    pub trait Split {
        fn js_split(&self, _on: &str) -> JsArray
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl Split for &str {}

    pub trait CharAt {
        // TODO can we type this to just be a char?
        fn char_at(&self, _index: i32) -> &str
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl CharAt for &str {}

    pub trait Length {
        // TODO can we type this to just be a char?
        fn length(&self) -> i32
        where
            Self: Sized,
        {
            todo!()
        }
    }
    impl Length for &str {}

    #[derive(Debug, Default)]
    pub struct Response {
        pub body: ResponseBody,
    }
    impl Response {
        pub async fn json<T>(&self) -> T {
            // let value_any = Box::new(()) as Box<dyn Any>;
            // *value_any.downcast::<T>().unwrap()
            todo!()
        }
    }

    #[derive(Debug, Default)]
    pub struct ResponseBody {}
    impl ResponseBody {
        pub fn pipe_through(
            &self,
            _text_decoder_stream: TextDecoderStream,
        ) -> TransformStreamReadableSide {
            TransformStreamReadableSide {}
        }
    }

    #[derive(Debug, Default)]
    pub struct TransformStreamReadableSide {}
    impl TransformStreamReadableSide {
        pub fn get_reader(&self) -> Reader {
            Reader::default()
        }
    }

    #[derive(Debug, Default)]
    pub struct Reader {}
    impl Reader {
        pub async fn read<T>(&self) -> T {
            todo!()
        }
    }

    #[derive(Debug, Default)]
    pub struct TextDecoderStream {}
    impl TextDecoderStream {
        pub fn new() -> TextDecoderStream {
            TextDecoderStream::default()
        }
    }

    #[derive(Debug, Default)]
    pub struct ObjectEntries {}
    impl Entries for ObjectEntries {}

    #[derive(Clone, Copy, Debug, Default)]
    pub struct Headers {}
    impl Headers {
        pub fn new() -> Headers {
            Headers::default()
        }
        pub fn append(&self, _key: &str, _value: &str) {}
        pub fn entries(&self) -> ObjectEntries {
            todo!()
        }
    }

    #[derive(Clone, Copy, Debug, Default)]
    pub struct Action {}

    #[derive(Clone, Copy, Debug)]
    pub enum Method {
        Delete,
        Get,
        Post,
        Put,
    }
    impl Default for Method {
        fn default() -> Self {
            Method::Get
        }
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch#body
    pub struct FetchBodyStruct {}
    pub trait FetchBody {}
    impl FetchBody for &str {}
    // impl FetchBody for FetchBodyStruct {}

    #[derive(Clone, Copy, Debug, Default)]
    pub struct FetchOptions<B: FetchBody + 'static> {
        method: Option<Method>,
        headers: Option<Headers>,
        body: Option<B>,
    }
    impl<B: FetchBody + 'static> FetchOptions<B> {
        pub fn new() -> FetchOptions<B> {
            FetchOptions {
                method: None,
                headers: None,
                body: None,
            }
        }
        pub fn method(mut self, method: Method) -> FetchOptions<B> {
            self.method = Some(method);
            self
        }
        pub fn headers(mut self, headers: Headers) -> FetchOptions<B> {
            self.headers = Some(headers);
            self
        }
        pub fn body(mut self, body: B) -> FetchOptions<B> {
            self.body = Some(body);
            self
        }
    }

    // pub trait FetchBody {}
    // impl FetchBody for &str {}
    // // impl FetchBody for FetchBodyStruct {}

    // pub struct FetchOptions {
    //     pub body: &'static dyn FetchBody,
    // }
    // impl FetchOptions {
    //     pub fn body(mut self, body: impl 'static + FetchBody) -> FetchOptions {
    //         self.body = &body;
    //         self
    //     }
    // }

    // pub trait FetchBody {}
    // impl FetchBody for &str {}
    // // impl FetchBody for FetchBodyStruct {}

    // pub struct FetchOptions<B: FetchBody + 'static> {
    //     pub body: B,
    // }

    // impl<B: FetchBody + 'static> FetchOptions<B> {
    //     pub fn body(mut self, body: B) -> FetchOptions<B> {
    //         self.body = body;
    //         self
    //     }
    // }

    #[derive(Clone, Copy, Debug)]
    pub struct Url {}
    /// https://developer.mozilla.org/en-US/docs/Web/API/fetch
    /// Default options are marked with *
    /// const response = await fetch(url, {
    ///     method: "POST", // *GET, POST, PUT, DELETE, etc.
    ///     mode: "cors", // no-cors, *cors, same-origin
    ///     cache: "no-cache", // *default, no-cache, reload, force-cache, only-if-cached
    ///     credentials: "same-origin", // include, *same-origin, omit
    ///     headers: {
    ///     "Content-Type": "application/json",
    ///     // 'Content-Type': 'application/x-www-form-urlencoded',
    ///     },
    ///     redirect: "follow", // manual, *follow, error
    ///     referrerPolicy: "no-referrer", // no-referrer, *no-referrer-when-downgrade, origin, origin-when-cross-origin, same-origin, strict-origin, strict-origin-when-cross-origin, unsafe-url
    ///     body: JSON.stringify(data), // body data type must match "Content-Type" header
    /// });
    ///
    /// Could stay closer to JS and use fetch!() which can take 1 or 2 arguments, Or build an API similar to reqwest?
    /// We are trading off easy to generate and predictable generated JS, familar to JS devs vs cleaner more idiotmatic Rust familiar to Rust devs
    /// Go with JS API for now otherwise it could be complicated to track potentially mutations to items over time, even with an AST
    /// Possibly best to go with JS at least at first so we don't back ourselves into a corner. And it is good to give people more direct control over the JS being output
    /// Using fetch!() means we can check the paths at compile time and only pass strings as path (not Url("/path")). But probably don't want hard coded paths anyway, they should all be coming from the backend/axum. But again, how would that be transpiled to JS? they could be CONST's which get output as const's, that then get inlined when code is optimised? But the strings will be in the backend code, how do we make that accessible? There is a few hacks we could use for Strings, but might we want to pass more complex data? Well it won't be program data like items with types, it will always be static data like str's (String's?) or other JSON serializable structures.
    /// We are going to want to share types like structs with methods anyway because they can represented in both JS and Rust and backend and frontend might have some logic they want to share (though might need to avoid using methods on builtin types), and so if the type complexity if already being shared, also sharing the data doesn't seem like a big stretch? eg might want something like `Paths` with `Paths::HOME` and `Path::product(id: usize)`.
    // pub struct Fetch {}
    // pub async fn fetch(_form_action: Url) -> Response {
    /// T is response json data type
    pub async fn fetch(_path: &str) -> Response {
        Response { body: todo!() }
    }

    // Could use macro rules to allow writing a json like object, but the fields are checked by the macro, and it gets transpiled to an actual object, but we loose the discoverability of methods. I think we can simply make the transpiler smart enough to convert a builder pattern -> setting fields on empty object -> defining object inline. It can do this automatically for built in structs, but might be easier to require anotating the struct with an attribute for user structs?
    // macro_rules! fetch_options {
    //     ($field_name:ident) => {};
    // }

    // pub async fn fetch2(_form_action: Url, _options: FetchOptions) -> Response {
    pub async fn fetch2<B: FetchBody + 'static, R>(
        _path: &str,
        _options: FetchOptions<B>,
    ) -> Response {
        Response { body: todo!() }
    }

    #[macro_export]
    macro_rules! try_ {
        ($try_block:block) => {{
            $try_block
        }};
    }
    #[macro_export]
    macro_rules! catch {
        ($err_ident:ident, $ErrType:ty, $catch_block:block) => {{
            let $err_ident: $ErrType = <$ErrType>::default();
            $catch_block
        }};
    }

    #[derive(Default)]
    pub struct SyntaxError {
        pub message: &'static str,
    }
    #[derive(Debug, Default)]
    pub struct Json {}
    impl Json {
        pub fn stringify(_object: impl JsonStringyArg) -> &'static str {
            todo!()
        }
        pub fn parse<T>(_text: &str) -> T {
            todo!()
        }
    }

    pub struct JsError {}
    pub fn try_(try_: impl Fn(), catch: impl Fn(JsError)) {
        todo!()
    }

    #[derive(Debug, Default)]
    pub struct FormDataEntries {}
    impl FormDataEntries {
        pub fn new(_dom_node: AnyNode) -> FormData {
            FormData::default()
        }
    }

    #[derive(Debug, Default)]
    pub struct FormData {}
    impl FormData {
        pub fn new(_dom_node: AnyNode) -> FormData {
            FormData::default()
        }
        pub fn entries(&self) -> FormDataEntries {
            FormDataEntries::default()
        }
    }
    pub trait Entries {}
    impl Entries for FormDataEntries {}

    pub trait JsonStringyArg {}
    impl JsonStringyArg for Object {}
    impl JsonStringyArg for &str {}

    #[derive(Debug, Default)]
    pub struct Object {}
    impl Object {
        pub fn from_entries(_entries: impl Entries) -> Object {
            Object::default()
        }
    }
    impl Index<&str> for Object {
        type Output = &'static str;

        fn index(&self, _key: &str) -> &Self::Output {
            &""
        }
    }

    #[derive(Debug)]
    pub struct Event {
        pub target: AnyNode,
    }
    impl Event {
        pub fn prevent_default(&self) {}
    }
    #[derive(Clone, Copy, Debug, Default)]
    pub struct ClassList {}
    impl ClassList {
        pub fn add(&self, _class_name: &str) {}
    }

    #[derive(Debug)]
    pub struct Console {}
    impl Console {
        pub fn log<T>(_to_log: T) {}
        pub fn assert(assertion: bool) {
            assert!(assertion);
        }
    }

    #[derive(Debug)]
    pub struct SseEvent {
        pub data: &'static str,
    }

    #[derive(Debug)]
    pub struct SseOptions {
        pub headers: Object,
        pub payload: &'static str,
    }

    #[derive(Debug)]
    pub struct Sse {}
    impl Sse {
        pub fn new(url: &str, options: SseOptions) -> Sse {
            todo!()
        }
        pub fn add_event_listener(&self, event_name: &str, callback: impl FnMut(SseEvent)) {
            todo!()
        }
    }

    #[derive(Clone, Copy, Debug, Default)]
    pub struct AnyNode {
        pub text_content: &'static str,
        pub class_list: ClassList,

        // TODO restrict these to Form dom nodes
        pub method: Method,
        // pub action: Action,
        // pub action: Url,
        pub action: &'static str,
        pub parent_element: Option<&'static AnyNode>,
    }

    pub trait Node {
        // TODO deprecate append_child() in favour of append_child2
        fn append_child3(&self, _child: AnyNode) {}
        fn append_child<T: Node>(&self, _a_child: T) {}
        fn add_event_listener(&self, _action: &str, _callback: impl Fn(Event)) {}
        fn add_event_listener_async<F>(&self, _action: &str, _callback: impl Fn(Event) -> F)
        where
            F: Future<Output = ()>,
        {
        }
        fn before<T: Node>(&self, _node: T) {}
    }
    impl Node for AnyNode {}

    pub trait Element: Node {
        fn set_attribute(&self, _attr_name: &str, _attr_val: &str) {}
        // TODO wait for async trait fns to stabilise
        // async fn request_fullscreen(&self) {}
        fn request_fullscreen(&self) {}
        // fn append_child(&self, child: DomNode);
        fn get_self() -> Self;
    }

    #[derive(Debug)]
    pub struct AnyElement {}
    impl Node for AnyElement {}
    impl Element for AnyElement {
        fn get_self() -> Self {
            AnyElement {}
        }
    }
    pub trait HtmlElement: Element {}
    pub struct AnyHtmlElement {}
    impl Node for AnyHtmlElement {}
    impl Element for AnyHtmlElement {
        fn get_self() -> Self {
            AnyHtmlElement {}
        }
    }
    impl HtmlElement for AnyHtmlElement {}

    pub struct Text {}
    impl Node for Text {}

    // pub trait DomNodeTrait {}
    // impl<T: DomNodeTrait> DomNodeTrait for Vec<T> {}

    pub struct Body {}
    impl Node for Body {}

    pub struct HTMLDivElement {}
    impl Node for HTMLDivElement {}
    impl Element for HTMLDivElement {
        fn get_self() -> Self {
            HTMLDivElement {}
        }
    }
    impl HtmlElement for HTMLDivElement {}

    pub struct Textarea {}
    impl Node for Textarea {}
    impl Element for Textarea {
        fn get_self() -> Self {
            Textarea {}
        }
    }
    impl HtmlElement for Textarea {}

    pub struct HTMLInputElement {
        pub value: &'static str,
    }
    impl Node for HTMLInputElement {}
    impl Element for HTMLInputElement {
        fn get_self() -> Self {
            HTMLInputElement { value: "" }
        }
    }
    impl HtmlElement for HTMLInputElement {}

    pub struct Date {
        pub iso_string: &'static str,
    }
    impl Date {
        pub fn from_iso_string(iso_string: &'static str) -> Date {
            Date { iso_string }
        }
    }

    pub struct Clipboard {}
    impl Clipboard {
        pub async fn write_text(&self, _new_clip_text: &str) {}
        pub async fn read_text(&self) -> &'static str {
            ""
        }
    }
    pub const NAVIGATOR: Navigator = Navigator {
        clipboard: Clipboard {},
    };
    // TODO how to force using NAVIGATOR not Navigator? could make clipboard private but then would need to convert clipboard() to clipboard
    pub struct Navigator {
        pub clipboard: Clipboard,
    }
    impl Navigator {
        pub const CLIPBOARD: Clipboard = Clipboard {};
    }
    // TODO it might be more ideal/idiomatic to implement like below, but would require more sohphisticated camel case handling
    // pub mod Navigator {
    //     pub struct Clipboard {}
    // }

    #[derive(Debug, Default)]
    pub struct Document {
        pub body: AnyNode,
    }
    impl Document {
        pub const DOCUMENT_ELEMENT: AnyElement = AnyElement {};
        // TODO need to convert these to actual booleans in the output JS
        // pub const FULLSCREEN_ELEMENT: Option<AnyElement> = None;
        pub const FULLSCREEN_ELEMENT: bool = false;

        /// TODO should return Option<DomNode> (except for Body) since js can null is there is no match
        pub fn query_selector(_selector: &str) -> AnyNode {
            AnyNode::default()
        }
        pub fn query_selector_body() -> Body {
            Body {}
        }
        pub fn query_selector2<T: Element>(_selector: &str) -> T {
            T::get_self()
        }
        pub fn get_element_by_id(_id: &str) -> AnyNode {
            AnyNode::default()
        }
        /// For type safety prefer to use `create_element_<tag name>`
        pub fn create_element(_tag: &str) -> impl HtmlElement {
            AnyHtmlElement {}
        }
        // pub fn create_element_div() -> HTMLDivElement {
        //     HTMLDivElement {}
        // }
        pub fn create_element_textarea() -> Textarea {
            Textarea {}
        }
        pub fn create_element2<T: Element>(_tag: &str) -> T {
            T::get_self()
        }
        pub fn create_text_node(_text: impl ToString) -> Text {
            Text {}
        }

        pub fn request_fullscreen() -> AnyNode {
            AnyNode::default()
        }
        pub const EXIT_FULLSCREEN: bool = false;
        pub fn exit_fullscreen() -> AnyNode {
            AnyNode::default()
        }
    }

    pub struct Timer {}
    pub fn set_interval(_callback: impl Fn(), _inc: i32) -> Timer {
        Timer {}
    }
    pub fn clear_interval(_timer: Timer) {}
}

mod typed {
    use super::web::*;
    impl Document {
        // TODO this doesn't seem to affect the visibility, think I need to use a feature flag or trait
        pub fn create_element_div() -> HTMLDivElement {
            HTMLDivElement {}
        }
    }
}

#[derive(Debug)]
pub struct Element {}
