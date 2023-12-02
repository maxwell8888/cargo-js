use heck::{AsKebabCase, AsLowerCamelCase, AsPascalCase};
use std::{
    fmt::Debug,
    fs,
    path::{Path, PathBuf},
};
use syn::{
    BinOp, Expr, FnArg, ImplItem, Item, ItemEnum, ItemFn, ItemMod, ItemUse, Lit, Member, Meta, Pat,
    Stmt, Type, UnOp, UseTree, Visibility,
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

fn handle_item_use_tree(use_tree: &UseTree, exports: &mut Vec<String>, module: &mut Vec<String>) {
    match use_tree {
        UseTree::Path(use_path) => {
            module.push(use_path.ident.to_string());
            handle_item_use_tree(&*use_path.tree, exports, module)
        }
        UseTree::Name(use_name) => exports.push(use_name.ident.to_string()),
        UseTree::Rename(_) => todo!(),
        UseTree::Glob(_) => todo!(),
        UseTree::Group(use_group) => use_group.items.iter().for_each(|item| match item {
            UseTree::Path(_) => todo!(),
            UseTree::Name(use_name) => exports.push(use_name.ident.to_string()),
            UseTree::Rename(_) => todo!(),
            UseTree::Glob(_) => todo!(),
            UseTree::Group(_) => todo!(),
        }),
    }
}

fn handle_item_use(item_use: &ItemUse) -> JsStmt {
    let mut exports = Vec::new();
    let mut module = Vec::new();
    let _is_pub = match item_use.vis {
        Visibility::Public(_) => true,
        _ => false,
    };
    handle_item_use_tree(&item_use.tree, &mut exports, &mut module);
    if module.iter().any(|seg| seg == "web") {
        if module.iter().any(|seg| seg == "Sse") {
            JsStmt::Raw(SSE_RAW_FUNC.to_string())
        } else {
            JsStmt::Expr(JsExpr::Vanish, false)
        }
    } else if module.get(0).unwrap() == "serde" || module.get(0).unwrap() == "serde_json" {
        JsStmt::Expr(JsExpr::Vanish, false)
    } else if module.get(0).unwrap() == "crate" {
        // If we import something from our crate, inline it (probably what we want for external crates too?)
        // A much simpler plan for now is to force defining the type in the JS file, and then export, rather than the other way round
        // Get the name of the item to be inlined
        todo!()
    } else {
        JsStmt::Import(None, exports, module)
    }
}

fn handle_stmt(stmt: &Stmt) -> JsStmt {
    match stmt {
        Stmt::Expr(expr, closing_semi) => JsStmt::Expr(handle_expr(expr), closing_semi.is_some()),
        Stmt::Local(local) => {
            //
            let names = match &local.pat {
                Pat::Ident(pat_ident) => vec![pat_ident.ident.to_string()],
                Pat::Tuple(pat_tuple) => pat_tuple
                    .elems
                    .iter()
                    .map(|elem| match elem {
                        Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
                        _ => todo!(),
                    })
                    .collect::<Vec<_>>(),
                other => {
                    dbg!(other);
                    todo!()
                }
            };
            let value = handle_expr(&*local.init.as_ref().unwrap().expr);
            match value {
                JsExpr::If(_assignment, _declare_var, condition, succeed, fail) => {
                    // TODO currently cases where the branch scope has a var with the same name as the result var means that the result will get assigned to that var, not the result var. Need to consider how to handle this. putting the branch lines inside a new `{}` scope and then doing the result assignment outside of this would work, but is ugly so would want to only do it where necessary, which would require iterating over the lines in a block to check for local declarations with that name.
                    JsStmt::Expr(
                        JsExpr::If(Some(names), true, condition, succeed, fail),
                        true,
                    )
                }

                value => JsStmt::Local(JsLocal {
                    type_: LocalType::Var,
                    destructure: LocalDestructure::None,
                    names,
                    value,
                }),
            }
        }
        Stmt::Item(item) => match item {
            // TODO this should all be handled by `fn handle_item()`
            Item::Const(_) => todo!(),
            Item::Enum(_) => todo!(),
            Item::ExternCrate(_) => todo!(),
            Item::Fn(item_fn) => handle_item_fn(item_fn),
            Item::ForeignMod(_) => todo!(),
            Item::Impl(_) => JsStmt::Expr(JsExpr::Vanish, false),
            Item::Macro(_) => todo!(),
            Item::Mod(_) => todo!(),
            Item::Static(_) => todo!(),
            Item::Struct(_) => JsStmt::Expr(JsExpr::Vanish, false),
            Item::Trait(_) => todo!(),
            Item::TraitAlias(_) => todo!(),
            Item::Type(_) => todo!(),
            Item::Union(_) => todo!(),
            Item::Use(item_use) => handle_item_use(item_use),
            Item::Verbatim(_) => todo!(),
            _ => todo!(),
        },
        Stmt::Macro(_) => todo!(),
    }
}

fn handle_item_fn(item_fn: &ItemFn) -> JsStmt {
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
    if ignore {
        JsStmt::Expr(JsExpr::Vanish, false)
    } else {
        JsStmt::Function(JsFn {
            export: match item_fn.vis {
                Visibility::Public(_) => true,
                _ => false,
            },
            async_: item_fn.sig.asyncness.is_some(),
            is_method: false,
            name: {
                let name = item_fn.sig.ident.to_string();
                AsLowerCamelCase(name).to_string()
            },
            input_names: item_fn
                .sig
                .inputs
                .iter()
                .map(|input| match input {
                    FnArg::Receiver(_) => todo!(),
                    FnArg::Typed(pat_type) => match &*pat_type.pat {
                        Pat::Ident(pat_ident) => {
                            AsLowerCamelCase(pat_ident.ident.to_string()).to_string()
                        }
                        _ => todo!(),
                    },
                })
                .collect::<Vec<_>>(),
            body_stmts: item_fn
                .block
                .stmts
                .iter()
                .map(|stmt| handle_stmt(stmt))
                .collect::<Vec<_>>(),
        })
    }
}

fn handle_item_enum(item_enum: ItemEnum) -> JsStmt {
    let mut static_fields = Vec::new();
    for variant in &item_enum.variants {
        static_fields.push(JsLocal {
            type_: LocalType::Static,
            destructure: LocalDestructure::None,
            names: vec![format!("{}Id", variant.ident.to_string())],
            value: JsExpr::LitStr(variant.ident.to_string()),
        })
    }

    let mut methods = Vec::new();
    for variant in &item_enum.variants {
        // dbg!(&variant);
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
    JsStmt::Class(JsClass {
        name: item_enum.ident.to_string(),
        inputs: Vec::new(),
        static_fields,
        methods,
    })
}

fn handle_item(item: Item, js_stmts: &mut Vec<JsStmt>) {
    match item {
        Item::Const(item_const) => js_stmts.push(JsStmt::Local(JsLocal {
            type_: LocalType::Var,
            destructure: LocalDestructure::None,
            names: vec![item_const.ident.to_string()],
            value: handle_expr(&*item_const.expr),
        })),
        Item::Enum(item_enum) => js_stmts.push(handle_item_enum(item_enum)),
        Item::ExternCrate(_) => todo!(),
        Item::Fn(item_fn) => js_stmts.push(handle_item_fn(&item_fn)),
        Item::ForeignMod(_) => todo!(),
        Item::Impl(item_impl) => {
            let class_name = match *item_impl.self_ty {
                Type::Path(type_path) => type_path.path.segments.first().unwrap().ident.to_string(),
                _ => todo!(),
            };
            for impl_item in item_impl.items {
                match impl_item {
                    ImplItem::Const(impl_item_const) => {
                        // impl_item_const
                        js_stmts.push(JsStmt::ClassStatic(JsLocal {
                            type_: LocalType::Static,
                            destructure: LocalDestructure::None,
                            names: vec![impl_item_const.ident.to_string()],
                            value: handle_expr(&impl_item_const.expr),
                        }))
                    }
                    ImplItem::Fn(item_impl_fn) => {
                        // dbg!(item_impl_fn.sig.ident.to_string());
                        // dbg!(item_impl_fn.sig.inputs.first().unwrap());
                        let static_ = match item_impl_fn.sig.inputs.first().unwrap() {
                            FnArg::Receiver(receiver) => false,
                            FnArg::Typed(_) => true,
                        };
                        // dbg!(static_);
                        let export = match item_impl_fn.vis {
                            Visibility::Public(_) => true,
                            Visibility::Restricted(_) => todo!(),
                            Visibility::Inherited => false,
                        };
                        let private = !export;
                        let input_names = item_impl_fn
                            .sig
                            .inputs
                            .into_iter()
                            .filter_map(|input| match input {
                                FnArg::Receiver(_) => None,
                                FnArg::Typed(pat_type) => match *pat_type.pat {
                                    Pat::Ident(pat_ident) => Some(
                                        AsLowerCamelCase(pat_ident.ident.to_string()).to_string(),
                                    ),
                                    _ => todo!(),
                                },
                            })
                            .collect::<Vec<_>>();
                        let body_stmts = item_impl_fn
                            .block
                            .stmts
                            .into_iter()
                            .map(|stmt| handle_stmt(&stmt))
                            .collect::<Vec<_>>();
                        js_stmts.push(JsStmt::ClassMethod(
                            class_name.clone(),
                            private,
                            static_,
                            JsFn {
                                is_method: true,
                                async_: item_impl_fn.sig.asyncness.is_some(),
                                export,
                                name: AsLowerCamelCase(item_impl_fn.sig.ident.to_string())
                                    .to_string(),
                                input_names,
                                body_stmts,
                            },
                        ))
                    }
                    ImplItem::Type(_) => todo!(),
                    ImplItem::Macro(_) => todo!(),
                    ImplItem::Verbatim(_) => todo!(),
                    _ => todo!(),
                }
            }
        }
        Item::Macro(_) => todo!(),
        Item::Mod(item_mod) => {
            let mut stmts = Vec::new();
            for thing in item_mod.content.unwrap().1 {
                handle_item(thing, &mut stmts);
            }
            js_stmts.push(JsStmt::ScopeBlock(stmts))
        }
        Item::Static(_) => todo!(),
        Item::Struct(item_struct) => {
            let js_stmt = JsStmt::Class(JsClass {
                name: item_struct.ident.to_string(),
                inputs: item_struct
                    .fields
                    .into_iter()
                    .map(|field| match field.ident {
                        Some(ident) => ident.to_string(),
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
        Item::Use(item_use) => js_stmts.push(handle_item_use(&item_use)),
        Item::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}

pub fn from_file_with_main(code: &str) -> Vec<JsStmt> {
    let mut js_stmts = from_file(code);
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

pub fn js_stmts_from_syn_items(items: Vec<Item>) -> Vec<JsStmt> {
    let mut js_stmts = Vec::new();
    for item in items {
        // dbg!(&item);
        handle_item(item, &mut js_stmts);
    }

    // Add methods from impl blocks to classes. This assumes impl blocks are at the top level
    let mut js_stmts2 = Vec::new();
    let mut my_class: Option<JsClass> = None;
    for stmt in js_stmts {
        match stmt {
            JsStmt::ClassStatic(js_local) => {
                if let Some(ref mut my_class) = my_class {
                    my_class.static_fields.push(js_local);
                } else {
                    panic!()
                }
            }
            JsStmt::ClassMethod(name, private, static_, js_fn) => {
                if let Some(ref mut my_class) = my_class {
                    if js_fn.name != "new" {
                        my_class.methods.push((name, private, static_, js_fn));
                    }
                } else {
                    panic!()
                }
            }
            JsStmt::Class(js_class) => {
                // If we already have a class set for updating, add it to stmts and this class as the new class for updating
                if let Some(my_class2) = my_class {
                    js_stmts2.push(JsStmt::Class(my_class2));
                }
                my_class = Some(js_class);
            }
            stmt => {
                // We must have finished adding class methods to class so push it to stmts now. We could just rely on the same code after the for loop, but have it here as well to ensure the order of statements is maintained
                if let Some(my_class2) = my_class {
                    js_stmts2.push(JsStmt::Class(my_class2));
                    my_class = None;
                }
                js_stmts2.push(stmt)
            }
        }
    }
    if let Some(my_class2) = my_class {
        js_stmts2.push(JsStmt::Class(my_class2));
    }

    js_stmts2
}

pub fn from_crate(file_path: PathBuf) -> Vec<JsStmt> {
    let code = fs::read_to_string(file_path).unwrap();
    let file = syn::parse_file(&code).unwrap();
    js_stmts_from_syn_items(file.items)
}

pub fn from_module(code: &str) -> Vec<JsStmt> {
    let item_mod = syn::parse_str::<ItemMod>(code).unwrap();
    let items = item_mod.content.unwrap().1;
    // dbg!(&items);
    js_stmts_from_syn_items(items)
}

pub fn from_file(code: &str) -> Vec<JsStmt> {
    let file = syn::parse_file(code).unwrap();
    js_stmts_from_syn_items(file.items)
}

pub fn from_fn(code: &str) -> Vec<JsStmt> {
    let item_fn = syn::parse_str::<ItemFn>(code).unwrap();

    let mut js_stmts = Vec::new();
    for stmt in &item_fn.block.stmts {
        let js_stmt = handle_stmt(stmt);
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
            BinOp::Ge(_) => todo!(),
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
    Assignment(Box<JsExpr>, Box<JsExpr>),
    /// (inputs, body)
    ArrowFn(Vec<String>, Vec<JsStmt>),
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
    Return(Box<JsExpr>),
    /// Will make the entire statement disappear no matter where it is nested?
    Vanish,
    /// (base var name, field name)
    Field(Box<JsExpr>, String),
    /// (name, args)
    FnCall(Box<JsExpr>, Vec<JsExpr>),
    /// `if else` statements are achieved by nesting an additional if statement as the fail arg.
    /// A problem is that Some assignment triggers a `var = x;`, however we also need to know whether we are doing assignment in nested If's (if else) but without adding a new var declaration. need to add another flag just to say when we need to declare the var
    /// (assignment, declare_var, condition, succeed, fail)
    If(
        Option<Vec<String>>,
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
    assignment: &Option<Vec<String>>,
    declare_var: &bool,
    cond: &Box<JsExpr>,
    succeed: &Vec<JsStmt>,
    // For some reason syn has an expr as the else branch, rather than the typical iter of statements - because the expr might be another if expr, not always a block
    fail: &Option<Box<JsExpr>>,
) -> String {
    let fail = if let Some(fail) = fail {
        match &**fail {
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
                let thing = match &**fail {
                    JsExpr::Block(stmts) => stmts
                        .iter()
                        .enumerate()
                        .map(|(i, stmt)| {
                            if i == stmts.len() - 1 {
                                if let Some(assignment) = assignment {
                                    format!(
                                        "{} = {};",
                                        assignment.first().unwrap(),
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
                    _ => {
                        if let Some(assignment) = assignment {
                            format!(
                                "{} = {};",
                                AsLowerCamelCase(assignment.first().unwrap()),
                                fail.js_string()
                            )
                        } else {
                            fail.js_string()
                        }
                    }
                };
                format!(" else {{\n{}\n}}", thing)
            }
        }
    } else {
        "".to_string()
    };
    let assignment_str = if let Some(names) = assignment {
        if *declare_var {
            let local = JsLocal {
                type_: LocalType::Var,
                destructure: LocalDestructure::None,
                names: names.clone(),
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

                        format!(
                            "{} = {};",
                            AsLowerCamelCase(assignment.first().unwrap()),
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
        fail
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
            JsExpr::ArrowFn(inputs, body) => {
                let sig = if inputs.len() == 1 {
                    inputs.get(0).unwrap().clone()
                } else {
                    format!("({})", inputs.join(", "))
                };
                let body = if body.len() == 1 {
                    // concise body
                    match body.get(0).unwrap() {
                        JsStmt::Local(_) => todo!(),
                        // TODO single objects returned by concise body must be wrapped in parenthesis
                        JsStmt::Expr(js_expr, closing_semi) => {
                            if *closing_semi {
                                panic!()
                            } else {
                                match js_expr {
                                    JsExpr::If(_, _, _, _, _) => {
                                        format!("{{\n{}\n}}", js_expr.js_string())
                                    }
                                    _ => js_expr.js_string(),
                                }
                            }
                        }
                        JsStmt::Import(_, _, _) => todo!(),
                        JsStmt::Function(_) => todo!(),
                        JsStmt::Class(_) => todo!(),
                        JsStmt::ClassMethod(_, _, _, _) => todo!(),
                        JsStmt::ClassStatic(_) => todo!(),
                        JsStmt::Raw(_) => todo!(),
                        JsStmt::ScopeBlock(_) => todo!(),
                    }
                } else {
                    format!(
                        "{{\n{}\n}}",
                        body.iter()
                            .map(|stmt| stmt.js_string())
                            .collect::<Vec<_>>()
                            .join("\n")
                    )
                };
                format!("{} => {}", sig, body)
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
        }
    }
}

// ::new()/Constructor must assign all fields of class
#[derive(Clone, Debug)]
pub struct JsClass {
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
pub enum LocalDestructure {
    None,
    Object,
    Array,
}
#[derive(Clone, Debug)]
pub struct JsLocal {
    type_: LocalType,
    destructure: LocalDestructure,
    names: Vec<String>,
    value: JsExpr,
}
impl JsLocal {
    fn js_string(&self) -> String {
        let original_name = self.names.get(0).unwrap().clone();
        let underscore_prefix = original_name.starts_with("_");
        let name = if self.names.len() == 1 {
            if underscore_prefix {
                format!("_{}", AsLowerCamelCase(original_name).to_string())
            } else {
                AsLowerCamelCase(original_name).to_string()
            }
        } else {
            match self.destructure {
                LocalDestructure::None => todo!(),
                LocalDestructure::Object => format!(
                    "{{ {} }}",
                    self.names
                        .iter()
                        .map(|name| {
                            let underscore_prefix = name.starts_with("_");
                            if underscore_prefix {
                                format!("_{}", AsLowerCamelCase(name).to_string())
                            } else {
                                AsLowerCamelCase(name).to_string()
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                LocalDestructure::Array => format!(
                    "[ {} ]",
                    self.names
                        .iter()
                        .map(|name| {
                            let underscore_prefix = name.starts_with("_");
                            if underscore_prefix {
                                format!("_{}", AsLowerCamelCase(name).to_string())
                            } else {
                                AsLowerCamelCase(name).to_string()
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            }
        };
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
            JsExpr::Blank => format!("{var_type} {name};"),
            value_js_expr => format!("{var_type} {name} = {};", value_js_expr.js_string()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct JsFn {
    export: bool,
    async_: bool,
    is_method: bool,
    name: String,
    input_names: Vec<String>,
    body_stmts: Vec<JsStmt>,
}
impl JsFn {
    fn js_string(&self) -> String {
        // dbg!(self);
        // TODO private fields and methods should be prepended with `#` like `#private_method() {}` but this would require also prepending all callsites of the the field or method, which requires more sophisticated AST analysis than we currently want to do.
        format!(
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
            self.body_stmts
                .iter()
                .enumerate()
                .map(|(i, stmt)| {
                    match stmt {
                        JsStmt::Local(js_local) => js_local.js_string(),
                        JsStmt::Expr(js_expr, semi) => match js_expr {
                            JsExpr::If(_, _, _, _, _) => js_expr.js_string(),
                            JsExpr::Block(_) => js_expr.js_string(),
                            JsExpr::While(_, _) => js_expr.js_string(),
                            JsExpr::ForLoop(_, _, _) => js_expr.js_string(),
                            _ => {
                                if *semi {
                                    format!("{};", js_expr.js_string())
                                } else if i == self.body_stmts.len() - 1 {
                                    format!("return {};", js_expr.js_string())
                                } else {
                                    js_expr.js_string()
                                }
                            }
                        },
                        JsStmt::Import(_, _, _) => todo!(),
                        JsStmt::Function(js_fn) => js_fn.js_string(),
                        JsStmt::Class(_) => todo!(),
                        JsStmt::ClassMethod(_, _, _, _) => todo!(),
                        JsStmt::ClassStatic(_) => todo!(),
                        JsStmt::Raw(text) => text.clone(),
                        JsStmt::ScopeBlock(stmts) => stmts
                            .iter()
                            .map(|stmt| stmt.js_string())
                            .collect::<Vec<_>>()
                            .join("\n"),
                    }
                })
                .collect::<Vec<_>>()
                .join("\n")
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
    Raw(String),
}

impl JsStmt {
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
                            .map(|export| AsLowerCamelCase(export).to_string())
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
        }
    }
}

fn handle_expr(expr: &Expr) -> JsExpr {
    match expr {
        Expr::Array(_) => todo!(),
        Expr::Assign(expr_assign) => JsExpr::Assignment(
            Box::new(handle_expr(&*expr_assign.left)),
            Box::new(handle_expr(&*expr_assign.right)),
        ),
        Expr::Async(_) => todo!(),
        Expr::Await(expr_await) => JsExpr::Await(Box::new(handle_expr(&*expr_await.base))),
        Expr::Binary(expr_binary) => JsExpr::Binary(
            Box::new(handle_expr(&*expr_binary.left)),
            JsOp::from_binop(expr_binary.op),
            Box::new(handle_expr(&*expr_binary.right)),
        ),
        Expr::Block(expr_block) => JsExpr::Block(
            expr_block
                .block
                .stmts
                .iter()
                .map(|stmt| handle_stmt(stmt))
                .collect::<Vec<_>>(),
        ),
        Expr::Break(_) => JsExpr::Break,
        Expr::Call(expr_call) => {
            let args = expr_call
                .args
                .iter()
                .map(|arg| handle_expr(arg))
                .collect::<Vec<_>>();
            match &*expr_call.func {
                Expr::Path(expr_path)
                    if expr_path.path.segments.last().unwrap().ident.to_string() == "fetch2" =>
                {
                    // TODO improve this code
                    JsExpr::FnCall(Box::new(JsExpr::Path(vec!["fetch".to_string()])), args)
                }
                Expr::Path(expr_path)
                    if expr_path.path.segments.last().unwrap().ident.to_string() == "new" =>
                {
                    // TODO improve this code
                    JsExpr::New(
                        expr_path
                            .path
                            .segments
                            .iter()
                            .take(expr_path.path.segments.len() - 1)
                            .map(|seg| seg.ident.to_string())
                            .collect::<Vec<_>>(),
                        args,
                    )
                }
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
                _ => JsExpr::FnCall(Box::new(handle_expr(&*expr_call.func)), args),
            }
        }

        Expr::Cast(_) => todo!(),
        Expr::Closure(expr_closure) => JsExpr::ArrowFn(
            expr_closure
                .inputs
                .iter()
                .map(|input| match input {
                    Pat::Ident(_) => todo!(),
                    Pat::Tuple(_) => todo!(),
                    Pat::Type(pat_type) => {
                        let name = match &*pat_type.pat {
                            Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
                            _ => todo!(),
                        };
                        AsLowerCamelCase(name).to_string()
                    }
                    other => {
                        dbg!(other);
                        todo!()
                    }
                })
                .collect::<Vec<_>>(),
            match &*expr_closure.body {
                Expr::Block(expr_block) => expr_block
                    .block
                    .stmts
                    .iter()
                    .map(|stmt| handle_stmt(stmt))
                    .collect::<Vec<_>>(),
                other => vec![JsStmt::Expr(handle_expr(other), false)],
            },
        ),
        Expr::Const(_) => todo!(),
        Expr::Continue(_) => todo!(),
        Expr::Field(expr_field) => JsExpr::Field(
            Box::new(handle_expr(&*expr_field.base)),
            match &expr_field.member {
                Member::Named(ident) => AsLowerCamelCase(ident.to_string()).to_string(),
                Member::Unnamed(_) => todo!(),
            },
        ),
        Expr::ForLoop(expr_for_loop) => JsExpr::ForLoop(
            match &*expr_for_loop.pat {
                Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
                _ => todo!(),
            },
            Box::new(handle_expr(&*expr_for_loop.expr)),
            expr_for_loop
                .body
                .stmts
                .iter()
                .map(|stmt| handle_stmt(&stmt))
                .collect::<Vec<_>>(),
        ),
        Expr::Group(_) => todo!(),
        Expr::If(expr_if) => JsExpr::If(
            None,
            false,
            Box::new(handle_expr(&*expr_if.cond)),
            expr_if
                .then_branch
                .stmts
                .iter()
                .map(|stmt| handle_stmt(stmt))
                .collect::<Vec<_>>(),
            expr_if
                .else_branch
                .as_ref()
                .map(|(_, expr)| Box::new(handle_expr(&*expr))),
        ),
        Expr::Index(expr_index) => JsExpr::Index(
            Box::new(handle_expr(&*expr_index.expr)),
            Box::new(handle_expr(&*expr_index.index)),
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
                .map(|stmt| handle_stmt(stmt))
                .collect::<Vec<_>>(),
        ),
        Expr::Macro(_) => todo!(),
        Expr::Match(expr_match) => {
            // (assignment, condition, succeed, fail)
            // TODO we need to know whether match result is being assigned to a var and therefore the if statement should be adding assignments to the end of each block
            let if_expr = expr_match.arms.iter().rev().fold(
                JsExpr::LitStr("this shouldn't exist".to_string()),
                |acc, arm| {
                    let (mut rhs, mut body_data_destructure) = match &arm.pat {
                        Pat::Const(_) => todo!(),
                        Pat::Ident(_) => todo!(),
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
                                    Member::Named(ident) => ident.to_string(),
                                    Member::Unnamed(_) => todo!(),
                                })
                                .collect::<Vec<_>>();
                            let stmt = JsStmt::Local(JsLocal {
                                type_: LocalType::Var,
                                destructure: LocalDestructure::Object,
                                names,
                                value: JsExpr::Field(
                                    Box::new(handle_expr(&*expr_match.expr)),
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
                                type_: LocalType::Var,
                                destructure: LocalDestructure::Array,
                                names,
                                value: JsExpr::Field(
                                    Box::new(handle_expr(&*expr_match.expr)),
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
                    };

                    // Need to take the path which will be eg [MyEnum, Baz], and convert to [MyEnum.bazId]
                    let index = rhs.len() - 1;
                    rhs[index] = format!("{}Id", AsLowerCamelCase(rhs[index].clone()).to_string());

                    let body = match &*arm.body {
                        // Expr::Array(_) => [JsStmt::Raw("sdafasdf".to_string())].to_vec(),
                        Expr::Array(_) => vec![JsStmt::Raw("sdafasdf".to_string())],
                        Expr::Block(expr_block) => expr_block
                            .block
                            .stmts
                            .iter()
                            .map(|stmt| handle_stmt(stmt))
                            .collect::<Vec<_>>(),
                        other_expr => {
                            vec![JsStmt::Expr(handle_expr(other_expr), false)]
                        }
                    };
                    body_data_destructure.extend(body.into_iter());
                    let body = body_data_destructure;

                    JsExpr::If(
                        None,
                        false,
                        Box::new(JsExpr::Binary(
                            Box::new(JsExpr::Field(
                                Box::new(handle_expr(&*expr_match.expr)),
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
        Expr::MethodCall(expr_method_call) => {
            let mut method_name = expr_method_call.method.to_string();
            let receiver = handle_expr(&*expr_method_call.receiver);

            if method_name.len() > 3 && &method_name[0..3] == "js_" {
                method_name = method_name[3..].to_string();
            }
            if method_name == "is_some" {
                return JsExpr::Binary(Box::new(receiver), JsOp::NotEq, Box::new(JsExpr::Null));
            }
            if method_name == "slice1" || method_name == "slice2" {
                method_name = "slice".to_string();
            }
            if method_name == "add_event_listener_async" {
                method_name = "add_event_listener".to_string();
            }
            if method_name == "length" {
                return JsExpr::Field(Box::new(receiver), "length".to_string());
            }
            JsExpr::MethodCall(
                Box::new(receiver),
                AsLowerCamelCase(method_name).to_string(),
                expr_method_call
                    .args
                    .iter()
                    .map(|arg| handle_expr(arg))
                    .collect::<Vec<_>>(),
            )
        }
        Expr::Paren(expr_paren) => JsExpr::Paren(Box::new(handle_expr(&*expr_paren.expr))),
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
                    if var_name.chars().all(|c| c.is_uppercase() || c == '_') {
                        AsLowerCamelCase(var_name).to_string()
                    } else if var_name.chars().next().unwrap().is_ascii_uppercase() {
                        AsPascalCase(var_name).to_string()
                    } else {
                        AsLowerCamelCase(var_name).to_string()
                    }
                })
                .collect::<Vec<_>>();
            JsExpr::Path(segs)
        }
        Expr::Range(_) => todo!(),
        Expr::Reference(expr_reference) => handle_expr(&*expr_reference.expr),
        Expr::Repeat(_) => todo!(),
        Expr::Return(expr_return) => {
            if let Some(expr) = &expr_return.expr {
                JsExpr::Return(Box::new(handle_expr(&*expr)))
            } else {
                JsExpr::Return(Box::new(JsExpr::Vanish))
            }
        }
        Expr::Struct(expr_struct) => {
            let struct_name = expr_struct.path.segments.first().unwrap().ident.to_string();
            if struct_name == "FetchOptions" || struct_name == "SseOptions" {
                JsExpr::Object(
                    expr_struct
                        .fields
                        .iter()
                        .map(|field| {
                            (
                                match &field.member {
                                    Member::Named(ident) => ident.to_string(),
                                    Member::Unnamed(_) => todo!(),
                                },
                                Box::new(handle_expr(&field.expr)),
                            )
                        })
                        .collect::<Vec<_>>(),
                )
            } else {
                // TODO we are assuming all other struct literals are inside ::new() so can be disappeared because the JsClass will write the constructor body
                JsExpr::Vanish
            }
        }
        Expr::Try(_) => todo!(),
        Expr::TryBlock(_) => todo!(),
        Expr::Tuple(_) => todo!(),
        Expr::Unary(expr_unary) => match expr_unary.op {
            UnOp::Deref(_) => handle_expr(&*expr_unary.expr),
            UnOp::Not(_) => JsExpr::Not(Box::new(handle_expr(&*expr_unary.expr))),
            UnOp::Neg(_) => JsExpr::Minus(Box::new(handle_expr(&*expr_unary.expr))),
            _ => todo!(),
        },
        Expr::Unsafe(_) => todo!(),
        Expr::Verbatim(_) => todo!(),
        Expr::While(expr_while) => JsExpr::While(
            Box::new(handle_expr(&*expr_while.cond)),
            expr_while
                .body
                .stmts
                .iter()
                .map(|stmt| handle_stmt(stmt))
                .collect::<Vec<_>>(),
        ),
        Expr::Yield(_) => todo!(),
        _ => todo!(),
    }
}

pub mod web {
    use std::{future::Future, ops::Index};

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
        Response::default()
    }

    // Could use macro rules to allow writing a json like object, but the fields are checked by the macro, and it gets transpiled to an actual object, but we loose the discoverability of methods. I think we can simply make the transpiler smart enough to convert a builder pattern -> setting fields on empty object -> defining object inline. It can do this automatically for built in structs, but might be easier to require anotating the struct with an attribute for user structs?
    // macro_rules! fetch_options {
    //     ($field_name:ident) => {};
    // }

    // pub async fn fetch2(_form_action: Url, _options: FetchOptions) -> Response {
    pub async fn fetch2<B: FetchBody + 'static>(
        _path: &str,
        _options: FetchOptions<B>,
    ) -> Response {
        Response::default()
    }

    #[derive(Debug, Default)]
    pub struct Json {}
    impl Json {
        pub fn stringify(_object: impl JsonStringyArg) -> &'static str {
            todo!()
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub struct DomNode {
        pub text_content: &'static str,
        pub class_list: ClassList,

        // TODO restrict these to Form dom nodes
        pub method: Method,
        // pub action: Action,
        // pub action: Url,
        pub action: &'static str,
        pub parent_element: &'static DomNode,
    }
    impl DomNode {
        pub fn append_child(&self, _child: DomNode) {}
        pub fn set_attribute(&self, _attr_name: &str, _attr_val: &str) {}
        pub fn add_event_listener(&self, _action: &str, _callback: impl Fn(Event)) {}
        pub fn add_event_listener_async<F>(&self, _action: &str, _callback: fn(Event) -> F)
        where
            F: Future<Output = ()> + 'static,
        {
        }
        pub fn before(&self, _node: DomNode) {}
    }

    #[derive(Debug, Default)]
    pub struct FormDataEntries {}
    impl FormDataEntries {
        pub fn new(_dom_node: DomNode) -> FormData {
            FormData::default()
        }
    }

    #[derive(Debug, Default)]
    pub struct FormData {}
    impl FormData {
        pub fn new(_dom_node: DomNode) -> FormData {
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
        pub target: DomNode,
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

    #[derive(Debug)]
    pub struct Document {
        pub body: DomNode,
    }
    impl Document {
        pub fn query_selector(_selector: &str) -> DomNode {
            // DomNode::default()
            todo!()
        }
        pub fn get_element_by_id(_id: &str) -> DomNode {
            // DomNode::default()
            todo!()
        }
        pub fn create_element(_tag: &str) -> DomNode {
            // DomNode::default()
            todo!()
        }
        pub fn create_text_node(_text: impl ToString) -> DomNode {
            // DomNode::default()
            todo!()
        }
    }

    pub struct Timer {}
    pub fn set_interval(_callback: impl Fn(), _inc: i32) -> Timer {
        Timer {}
    }
    pub fn clear_interval(_timer: Timer) {}
}

#[derive(Debug)]
pub struct Element {}
