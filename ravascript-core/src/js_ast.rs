use core::fmt;
use heck::{AsKebabCase, AsPascalCase};
// use std::io::{self, Write};
use syn::BinOp;

use crate::{camel, case_convert};

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
    /// TODO shouldn't need to specify block, should just be able to infer from number of stmts/exprs when rendering?
    ArrowFn(bool, bool, Vec<Ident>, Vec<JsStmt>),
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
    /// (base expr, field name)
    Field(Box<JsExpr>, Ident),
    Fn(JsFn),
    /// (name, args)
    FnCall(Box<JsExpr>, Vec<JsExpr>),
    /// (pat, expr, block)
    ForLoop(Ident, Box<JsExpr>, Vec<JsStmt>),
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
    MethodCall(Box<JsExpr>, Ident, Vec<JsExpr>),
    Minus(Box<JsExpr>),
    /// (Class path, args)
    New(PathIdent, Vec<JsExpr>),
    Null,
    Not(Box<JsExpr>),
    Object(Vec<(String, Box<JsExpr>)>),
    ObjectForModule(Vec<JsStmt>),
    Paren(Box<JsExpr>),
    /// eg module::struct::associated_fn -> module.struct.associated_fn;
    /// NOTE each path segment is a Vec<String> rather than a String because the name might be a deduplicated and so needs to be joined
    /// This seems inefficient given 99% of cases will be len=1 so we are adding extra allocations/indirection for no reason. Just use an Enum instead
    Path(PathIdent),
    Raw(String),
    Return(Box<JsExpr>),
    ThrowError(String),
    /// Will make the entire statement disappear no matter where it is nested?
    Vanish,
    Var(String),
    // Class(JsClass),
    While(Box<JsExpr>, Vec<JsStmt>),
    TryBlock(Vec<JsStmt>),
    CatchBlock(String, Vec<JsStmt>),
}
impl fmt::Display for JsExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsExpr::LitInt(int) => write!(f, "{int}"),
            // NOTE `(5.).to_string()` is "5" not "5." or "5.0"
            JsExpr::LitFloat(float) => write!(f, "{float}"),
            JsExpr::LitStr(text) => write!(f, r#""{text}""#),
            JsExpr::LitBool(bool) => write!(f, "{bool}"),
            JsExpr::Object(fields) => {
                writeln!(f, "{{",)?;
                for (i, (member, expr)) in fields.iter().enumerate() {
                    write!(f, "{member}: {expr}")?;
                    if i != fields.len() - 1 {
                        writeln!(f, ",",)?;
                    }
                }
                // writeln!(
                //     writer,
                //     "{{\n{}\n}}",
                //     fields
                //         .iter()
                //         .map(|(member, expr)| format!("{member}: {}", expr.write_js()))
                //         .collect::<Vec<_>>()
                //         .join(",\n")
                // );
                write!(f, "}}",)
            }
            JsExpr::Vanish => Ok(()),
            JsExpr::Blank => Ok(()),
            JsExpr::FnCall(func, args) => write!(f, "{func}({})", args.fmt_join(", ")),
            JsExpr::ArrowFn(async_, block, inputs, body) => {
                // JS formatter converts `x => x` to `(x) => x` which seems pointless but what we will go with for now
                // let sig = if inputs.len() == 1 {
                //     inputs.get(0).unwrap().clone()
                // } else {
                //     format!("({})", inputs.join(", "))
                // };

                // let sig = format!("({})", inputs.join(", "));

                // // TODO single objects returned by concise body must be wrapped in parenthesis
                // let body = if *block {
                //     body.iter()
                //         .enumerate()
                //         .map(|(i, stmt)| handle_fn_body_stmt(i, stmt, body.len()))
                //         .collect::<Vec<_>>()
                //         .join("\n")
                // } else if body.len() > 1 {
                //     panic!("closures with no block should only have 1 statement")
                // } else {
                //     body.first().unwrap().js_string()
                // };
                // format!(
                //     "{}{} => {}",
                //     if *async_ { "async " } else { "" },
                //     sig,
                //     if *block {
                //         format!("{{\n{}\n}}", body)
                //     } else {
                //         body
                //     }
                // )

                let sig = format!("({})", inputs.fmt_join(", "));

                write!(f, "{}{} => ", if *async_ { "async " } else { "" }, sig,)?;

                // TODO single objects returned by concise body must be wrapped in parenthesis
                if *block {
                    writeln!(f, "{{")?;
                    for (i, stmt) in body.iter().enumerate() {
                        handle_fn_body_stmt(f, i, stmt, body.len())?;
                        if i != body.len() - 1 {
                            writeln!(f)?;
                        }
                    }
                    writeln!(f, "\n}}")
                } else if body.len() > 1 {
                    panic!("closures with no block should only have 1 statement")
                } else {
                    write!(f, "{}", body.first().unwrap())
                }
            }
            JsExpr::Binary(left, op, right) => {
                // write!(
                //     writer,
                //     "{} {} {}",
                //     left.write_js(),
                //     op.js_string(),
                //     right.write_js()
                // )
                write!(f, "{left} {} {right}", op.js_string())
            }
            JsExpr::Var(var_name) => write!(f, "{var_name}"),
            JsExpr::Field(base, field_name) => write!(f, "{base}.{field_name}"),
            JsExpr::New(path, args) => {
                // format!(
                //     "new {}({})",
                //     path.join("."),
                //     args.iter()
                //         .map(|arg| arg.write_js())
                //         .collect::<Vec<_>>()
                //         .join(", ")
                // )

                write!(f, "new {path}({})", args.fmt_join(", "))
            }
            JsExpr::MethodCall(receiver_name, method_name, args) => {
                // format!(
                //     "{}.{}({})",
                //     receiver_name.write_js(),
                //     method_name,
                //     args.iter()
                //         .map(|arg| arg.write_js())
                //         .collect::<Vec<_>>()
                //         .join(", ")
                // )
                write!(
                    f,
                    "{receiver_name}.{}({})",
                    camel(method_name),
                    args.fmt_join(", ")
                )
            }
            JsExpr::Path(segments) => {
                // segments.join(".")
                write!(f, "{segments}")
            }
            JsExpr::Assignment(left, right) => write!(f, "{left} = {right}"),
            JsExpr::ForLoop(pat, expr, block) => {
                // format!(
                //     "for (var {pat} of {}) {{\n{}\n}}",
                //     expr.write_js(),
                //     block
                //         .iter()
                //         .map(|expr| expr.js_string())
                //         .collect::<Vec<_>>()
                //         .join("\n")
                // )
                write!(
                    f,
                    "for (var {pat} of {expr}) {{{}\n}}",
                    block.fmt_join("\n")
                )
            }
            JsExpr::Index(expr, index) => write!(f, "{expr}[{index}]"),
            JsExpr::Await(expr) => write!(f, "await {expr}"),
            JsExpr::While(cond, block) => {
                // format!(
                //     "while ({}) {{\n{}\n}}",
                //     cond.write_js(),
                //     block
                //         .iter()
                //         .map(|stmt| stmt.js_string())
                //         .collect::<Vec<_>>()
                //         .join("\n")
                // )
                write!(f, "while ({cond}) {{{}\n}}", block.fmt_join("\n"))
            }
            JsExpr::If(js_if) => write!(f, "{js_if}"),
            JsExpr::Declaration(_, name, expr) => write!(f, "var {name} = {expr}"),
            JsExpr::Break => write!(f, "break"),
            JsExpr::Not(expr) => write!(f, "!{expr}"),
            JsExpr::Block(stmts) => {
                // let stmts = stmts
                //     .iter()
                //     .map(|stmt| stmt.js_string())
                //     .collect::<Vec<_>>()
                //     .join("\n");
                // format!("{{\n{}\n}}", stmts)
                write!(f, "{{\n{}\n}}", stmts.fmt_join("\n"))
            }
            JsExpr::Minus(expr) => write!(f, "-{expr}"),
            JsExpr::Paren(expr) => write!(f, "({expr})"),
            JsExpr::Null => write!(f, "null"),
            JsExpr::Return(expr) => write!(f, "return {expr}"),
            JsExpr::Array(elems) => {
                // format!(
                //     "[{}]",
                //     elems
                //         .iter()
                //         .map(|elem| elem.write_js())
                //         .collect::<Vec<_>>()
                //         .join(", ")
                // )
                write!(f, "[{}]", elems.fmt_join(", "))
            }
            JsExpr::Fn(js_fn) => write!(f, "{js_fn}"),
            JsExpr::ObjectForModule(js_stmts) => {
                let js_stmt_module = JsModule {
                    public: false,
                    name: "whatever".to_string(),
                    module_path: vec![],
                    stmts: js_stmts.clone(),
                };
                write!(f, "{js_stmt_module}")
            }
            JsExpr::Raw(text) => write!(f, "{text}"),
            JsExpr::ThrowError(message) => {
                // TODO improve this - ideally use existing code eg from rustc

                let parts = message
                    .split(',')
                    .map(|part| part.trim().to_string())
                    .collect::<Vec<_>>();
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
                } else if parts[0].starts_with('"') {
                    parts[0].clone()
                } else {
                    format!(r#""{}""#, parts[0])
                };

                // let lhs = parts.next().unwrap();
                // let lhs = syn::parse_str::<syn::Expr>(lhs).unwrap();
                // let lhs = handle_expr(&lhs, global_data, current_module_path);

                write!(f, r#"throw new Error({expanded_message})"#)
            }
            JsExpr::TryBlock(try_block) => {
                // format!(
                //     "try {{\n{}\n}}",
                //     try_block
                //         .iter()
                //         .map(|s| s.js_string())
                //         .collect::<Vec<_>>()
                //         .join("\n"),
                // )
                write!(f, "try {{\n{}\n}}", try_block.fmt_join("\n"))
            }
            JsExpr::CatchBlock(err_var_name, catch_block) => {
                // format!(
                //     "catch ({}) {{\n{}\n}}",
                //     err_var_name,
                //     catch_block
                //         .iter()
                //         .map(|s| s.js_string())
                //         .collect::<Vec<_>>()
                //         .join("\n")
                // )
                writeln!(
                    f,
                    "catch ({err_var_name}) {{\n{}\n}}",
                    catch_block.fmt_join("\n")
                )
            }
        }
    }
}

// #[derive(Clone, Debug, PartialEq)]
#[derive(Clone, Debug)]
pub enum Ident {
    Syn(syn::Ident),
    String(String),
    Str(&'static str),
    Deduped(Vec<String>),
    // TODO this should be a wrapper or something rather than a variant so we can use str or String or syn::Ident
    NoConversion(String),
}
impl PartialEq<String> for Ident {
    fn eq(&self, other: &String) -> bool {
        match self {
            Ident::Syn(ident) => ident == other,
            Ident::String(ident) => ident == other,
            Ident::Str(ident) => ident == other,
            Ident::Deduped(_idents) => todo!(),
            Ident::NoConversion(ident) => ident == other,
        }
    }
}
impl PartialEq<&'static str> for Ident {
    fn eq(&self, other: &&'static str) -> bool {
        match self {
            Ident::Syn(ident) => ident == other,
            Ident::String(ident) => ident == other,
            Ident::Str(ident) => ident == other,
            Ident::Deduped(_idents) => todo!(),
            Ident::NoConversion(ident) => ident == other,
        }
    }
}
impl fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ident::Syn(ident) => write!(f, "{}", case_convert(ident)),
            Ident::String(ident) => write!(f, "{}", case_convert(ident)),
            Ident::Str(ident) => write!(f, "{}", case_convert(ident)),
            Ident::Deduped(idents) => write!(
                f,
                "{}",
                idents
                    .iter()
                    // TODO avoid needing to convert to String
                    .map(|ident| case_convert(ident))
                    .collect::<Vec<_>>()
                    .join("__")
            ),
            Ident::NoConversion(ident) => write!(f, "{ident}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum PathIdent {
    Single(Ident),
    Path(Vec<Ident>),
    // TODO support using arrays instead of Vec for known size paths, to avoid allocations
    PathTwo([Ident; 2]),
}
impl From<&'static str> for PathIdent {
    fn from(value: &'static str) -> Self {
        PathIdent::Single(Ident::Str(value))
    }
}
impl From<syn::Ident> for PathIdent {
    fn from(value: syn::Ident) -> Self {
        PathIdent::Single(Ident::Syn(value))
    }
}

impl PathIdent {
    pub fn len(&self) -> usize {
        match self {
            PathIdent::Single(_) => 1,
            PathIdent::Path(idents) => idents.len(),
            PathIdent::PathTwo(_) => 2,
        }
    }
}

impl fmt::Display for PathIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathIdent::Single(ident) => write!(f, "{ident}"),
            PathIdent::Path(idents) => write!(
                f,
                "{}",
                idents
                    .iter()
                    // TODO avoid needing to convert to String
                    .map(|ident| ident.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            ),
            PathIdent::PathTwo(idents) => write!(f, "{}.{}", idents[0], idents[1]),
        }
    }
}

// impl fmt::Display for Transfer {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         let Transfer {
//             source_owner,
//             destination_owner,
//             formatted_amount,
//         } = self;
//         write!(
//             f,
//             "TX detected: {source_owner} sent {formatted_amount} USDC to {destination_owner}"
//         )
//     }
// }

// fn fmt_join<T: fmt::Display, Seperator: fmt::Display>(
//     f: &mut fmt::Formatter<'_>,
//     slice: &[T],
//     sep: Seperator,
// ) -> fmt::Result {
//     for (i, item) in slice.iter().enumerate() {
//         write!(f, "{item}")?;
//         if i != slice.len() - 1 {
//             write!(f, "{sep}")?;
//         }
//     }
//     Ok(())
// }

pub trait FmtExtensions<'a, T: fmt::Display, Seperator: fmt::Display> {
    fn fmt_join(&'a self, sep: Seperator) -> DisplayIter<'a, T, Seperator>;
}
pub struct DisplayIter<'a, T: fmt::Display, Seperator: fmt::Display> {
    slice: &'a [T],
    sep: Seperator,
}
impl<'a, T: fmt::Display, Seperator: fmt::Display> fmt::Display for DisplayIter<'a, T, Seperator> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.slice.iter().enumerate() {
            write!(f, "{item}")?;
            if i != self.slice.len() - 1 {
                write!(f, "{}", self.sep)?;
            }
        }
        Ok(())
    }
}
impl<'a, T: fmt::Display, Seperator: fmt::Display> FmtExtensions<'a, T, Seperator> for &[T] {
    fn fmt_join(&'a self, sep: Seperator) -> DisplayIter<'a, T, Seperator> {
        DisplayIter { slice: self, sep }
    }
}
impl<'a, T: fmt::Display, Seperator: fmt::Display> FmtExtensions<'a, T, Seperator> for Vec<T> {
    fn fmt_join(&'a self, sep: Seperator) -> DisplayIter<'a, T, Seperator> {
        DisplayIter { slice: self, sep }
    }
}

// impl JsExpr {
//     fn write_js<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {}
// }

#[derive(Clone, Debug)]
pub struct JsIf {
    /// The name of the initialised var we are assigning to in the final statement of the block
    pub assignment: Option<LocalName>,
    /// Whether to prepend the if statement with eg `var result = `
    /// TODO not sure why this is necessary and not handle by the original `let` `Local` statement
    pub declare_var: bool,
    pub condition: Box<JsExpr>,
    pub succeed: Vec<JsStmt>,
    /// we copy syn here which has an expr as the else branch, rather than an iter of statements because the expr might be another if expr, not always a block
    pub fail: Option<Box<JsExpr>>,
}

// TODO Make a struct called If with these fields so I can define js_string() on the struct and not have this fn
// TODO we want to know if the if is a single statment being return from a fn and if so in line the return in the branches rather than use an assignment var
impl JsIf {
    // fn if_expr_to_string(&self) -> String {}
}
impl fmt::Display for JsIf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let JsIf {
            assignment,
            declare_var,
            condition: cond,
            succeed,
            fail: else_,
        } = self;

        if let Some(lhs) = assignment {
            if *declare_var {
                let local = JsLocal {
                    public: false,
                    export: false,
                    type_: LocalType::Var,
                    lhs: lhs.clone(),
                    value: JsExpr::Blank,
                };
                writeln!(f, "{local}")?;
            }
        }

        writeln!(f, "if ({cond}) {{")?;
        for (i, stmt) in succeed.iter().enumerate() {
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

                    let is_error = matches!(stmt, JsStmt::Expr(JsExpr::ThrowError(_), _));
                    if is_error {
                        write!(f, "{stmt}")?;
                    } else {
                        write!(f, "{assignment} = {stmt};")?;
                    }
                } else {
                    write!(f, "{stmt}")?;
                }
            } else {
                write!(f, "{stmt}")?;
            }
        }

        write!(f, "\n}}")?;

        if let Some(else_) = else_ {
            match &**else_ {
                // if else {}
                JsExpr::If(js_if) => {
                    write!(
                        f,
                        " else {}",
                        JsExpr::If(JsIf {
                            assignment: assignment.clone(),
                            declare_var: false,
                            condition: js_if.condition.clone(),
                            succeed: js_if.succeed.clone(),
                            fail: js_if.fail.clone()
                        })
                    )?;
                }
                // else {}
                _ => {
                    writeln!(f, " else {{")?;

                    match &**else_ {
                        // else { block of stmts }
                        JsExpr::Block(stmts) => {
                            for (i, stmt) in stmts.iter().enumerate() {
                                if i == stmts.len() - 1 {
                                    if let Some(assignment) = assignment {
                                        #[allow(clippy::all)]
                                        let is_error = match stmt {
                                            JsStmt::Expr(expr, _) => match expr {
                                                JsExpr::ThrowError(_) => todo!(),
                                                _ => false,
                                            },
                                            _ => false,
                                        };
                                        write!(f, "{assignment} = {stmt};")?;
                                    } else {
                                        write!(f, "{stmt}")?;
                                    }
                                } else {
                                    writeln!(f, "{stmt}")?;
                                }
                            }
                        }
                        // else { expr }
                        _ => {
                            if let Some(assignment) = assignment {
                                let is_error = matches!(&**else_, JsExpr::ThrowError(_));
                                // let is_error = match stmt {
                                //     JsStmt::Expr(expr, _) => match expr {
                                //         JsExpr::ThrowError(_) => true,
                                //         _ => false,
                                //     },
                                //     _ => false,
                                // };
                                if is_error {
                                    write!(f, "{else_};")?;
                                } else {
                                    // format!("{} = {};", assignment.js_string(), stmt.js_string())
                                    write!(f, "{assignment} = {else_};")?;
                                }
                            } else {
                                write!(f, "{else_}")?;
                            }
                        }
                    };

                    write!(f, "\n}}")?;
                }
            }
        }

        Ok(())
    }
}

// ::new()/Constructor must assign all fields of class
#[derive(Clone, Debug)]
pub struct JsClass {
    /// None if class is not defined at module level
    // module_path: Option<Vec<String>>,
    pub public: bool,
    pub export: bool,
    pub tuple_struct: bool,
    // This should be camel cased because it is too late to get the deduped "fully qualified" name at this point
    pub name: Ident,
    /// we are assuming input names is equivalent to field names
    pub inputs: Vec<Ident>,
    pub static_fields: Vec<JsLocal>,
    /// (class name, static, JsFn)  
    pub methods: Vec<(Ident, bool, JsFn)>,
    // NOTE dropped the idea of just storing eg a list of impld items or the path to an impl block, because there is methods and fields we might want to add manually eg for an enum, so it makes more sense
    // struct_or_enum: StructOrEnumSynObject,
    // /// all methods impl'd specifically for the type ie `impl MyStructOrEnum { ... }` and `impl MyTrait for MyStructOrEnum { ... }`
    // impld_methods: Vec<JsImplItem>,
    // /// all methods impl'd for a generic that matches this type ie `impl<T> MyTrait for T { ... }`
    // /// trait impl block name, which can be transpiled/formatted to eg `var someModule__myTrait__for__anotherModule__myStructOrEnum = { ... }`
    // /// (trait namespaced name, type namespaced name) (namespaced name just means a vector of snake_case, hasn't bean camel cased or joined with __ yet.
    // generic_trait_impl_methods: Vec<(Vec<String>, Vec<String>)>,
    /// We need this because after syn -> JS parsing a block (scoped impls) or crate (module level impls) we need to go through and update all the `JsClass`s with their `impl`d methods, either directly add the method for direct impls, or a link the the trait impl object for generic impls, and for the later case we need to check if the classes type matches the trait impl (actually I think we need the type params from the ItemDefinition for simple direct impls too if the item is generic and has bounds, we are probably just glossing over this at the moment).
    pub rust_name: String,
    pub module_path: Vec<String>,
    pub scope_id: Option<Vec<usize>>,
    /// TODO Should be an enum containing rust_name and module_path
    pub is_impl_block: bool,
}
// #[derive(Clone, Debug)]
// pub struct JsClassImpldMethods {
//     // TypeImplItem(TypeImplItem),
//     // TraitImplItem(TraitImplItem),
//     // TypeImplItem(ImplItem),
//     // TraitImplItem(ImplItem),
//     impl_item: ImplItem,
//     impl_type_or_trait: ImplTypeOrTrait
// }

// #[derive(Clone, Debug)]
// pub enum ImplTypeOrTrait {
//     Type,
//     /// trait impl block name, which can be transpiled/formatted to eg `var someModule__myTrait__for__anotherModule__myStructOrEnum = { ... }`
//     /// (trait namespaced name, type namespaced name) (namespaced name just means a vector of snake_case, hasn't bean camel cased or joined with __ yet.
//     Trait(Vec<String>, Vec<String>)
// }
// #[derive(Clone, Debug)]
// pub struct TypeImplItem {

// }

#[derive(Clone, Debug)]
pub enum LocalType {
    // TODO None probably doesn't belong here, it is just a quick hack to allow non-static fields in classes
    None,
    Var,
    Const,
    Let,
    Static,
}

#[derive(Clone, Debug)]
pub enum DestructureValue {
    /// A simple destructure like `var { a } = obj;`
    KeyName(Ident),
    /// A rename destructure like `var { a: b } = obj;`
    Rename(Ident, Ident),
    /// A nested destructure like `var { a: { b } } = obj;`
    Nesting(Ident, DestructureObject),
}
impl DestructureValue {
    // fn js_string(&self) -> String {}
}
impl fmt::Display for DestructureValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DestructureValue::KeyName(key) => write!(f, "{key}"),
            DestructureValue::Rename(key, new_name) => write!(f, "{key}: {new_name}"),
            DestructureValue::Nesting(key, destructure_object) => {
                write!(f, "{key}: {destructure_object}")
            }
        }
    }
}

#[derive(Clone, Debug)]
// TODO consider replacing the Vec with DestructureValue::Group, like how syn works
pub struct DestructureObject(pub Vec<DestructureValue>);
impl DestructureObject {
    // fn js_string(&self) -> String {}
}
impl fmt::Display for DestructureObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ {} }}", self.0.fmt_join(", "))
    }
}

#[derive(Clone, Debug)]
pub enum LocalName {
    Single(Ident),
    DestructureObject(DestructureObject),
    DestructureArray(Vec<LocalName>),
}
impl LocalName {
    // fn js_string(&self) -> String {}
}
impl fmt::Display for LocalName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LocalName::Single(name) => write!(f, "{name}"),
            LocalName::DestructureObject(destructure_object) => write!(f, "{destructure_object}"),
            LocalName::DestructureArray(destructure_array) => {
                write!(f, "[ {} ]", destructure_array.fmt_join(", "))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct JsLocal {
    pub public: bool,
    pub export: bool,
    pub type_: LocalType,
    pub lhs: LocalName,
    pub value: JsExpr,
}
impl JsLocal {
    // fn js_string(&self) -> String {}
}
impl fmt::Display for JsLocal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lhs = &self.lhs;
        let var_type = match self.type_ {
            LocalType::None => "",
            LocalType::Var => "var",
            LocalType::Const => "const",
            LocalType::Let => "let",
            LocalType::Static => "static",
        };

        // check if there is any shadowing in scope and use var instead
        match &self.value {
            // TODO what if we want to declare a null var like `var myvar;` eg prior to if statement
            JsExpr::Vanish => Ok(()),
            JsExpr::Blank => write!(f, "{var_type} {lhs};"),
            value_js_expr => write!(f, "{var_type} {lhs} = {value_js_expr};"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct JsFn {
    pub iife: bool,
    pub public: bool,
    pub export: bool,
    pub async_: bool,
    pub is_method: bool,
    // Camel case?
    pub name: Ident,
    pub input_names: Vec<Ident>,
    pub body_stmts: Vec<JsStmt>,
}

/// Adds return and semi to expr being returned. For an if expression this means we also need to get the name of the assignment var that needs returning  
fn handle_fn_body_stmt(
    f: &mut fmt::Formatter<'_>,
    i: usize,
    stmt: &JsStmt,
    len: usize,
) -> fmt::Result {
    match stmt {
        JsStmt::Local(js_local) => write!(f, "{js_local}"),
        JsStmt::Expr(js_expr, semi) => match js_expr {
            JsExpr::If(js_if) => {
                if i == len - 1 {
                    // TODO wrongly assuming that all single if expr bodys should be returned
                    if let Some(assignment) = &js_if.assignment {
                        write!(f, "{js_expr}\nreturn {assignment};")
                    } else {
                        write!(f, "{js_expr}")
                    }
                } else {
                    write!(f, "{js_expr}")
                }
            }
            JsExpr::Block(_) => write!(f, "{js_expr}"),
            JsExpr::While(_, _) => write!(f, "{js_expr}"),
            JsExpr::ForLoop(_, _, _) => write!(f, "{js_expr}"),
            JsExpr::Vanish => Ok(()),
            _ => {
                if *semi {
                    write!(f, "{js_expr};")
                } else if i == len - 1 {
                    write!(f, "return {js_expr};")
                } else {
                    write!(f, "{js_expr}")
                }
            }
        },
        JsStmt::Import(_, _, _) => todo!(),
        JsStmt::Function(_) => write!(f, "{stmt}"),
        JsStmt::Class(_) => write!(f, "{stmt}"),
        JsStmt::ClassMethod(_, _, _, _) => write!(f, "{stmt}"),
        JsStmt::ClassStatic(_) => write!(f, "{stmt}"),
        JsStmt::Raw(_) => write!(f, "{stmt}"),
        JsStmt::ScopeBlock(_) => write!(f, "{stmt}"),
        // JsStmt::TryBlock(_) => stmt.js_string(),
        // JsStmt::CatchBlock(_, _) => stmt.js_string(),
        JsStmt::Comment(_) => write!(f, "{stmt}"),
    }
}
impl JsFn {
    // fn js_string(&self) -> String {}
}
impl fmt::Display for JsFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.iife {
            write!(f, "(")?;
        }

        // TODO private fields and methods should be prepended with `#` like `#private_method() {}` but this would require also prepending all callsites of the the field or method, which requires more sophisticated AST analysis than we currently want to do.
        write!(
            f,
            "{}{}{}{}({}) {{\n{}\n}}",
            if self.export && !self.is_method {
                "export default "
            } else {
                ""
            },
            if self.async_ { "async " } else { "" },
            if self.is_method { "" } else { "function " },
            self.name,
            self.input_names.fmt_join(", "),
            self.body_stmts.fmt_join("\n")
        )?;

        if self.iife {
            write!(f, ")()")?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct JsModule {
    pub public: bool,
    /// camelCase JS name
    pub name: String,
    /// snake_case Rust path starting with "crate"
    pub module_path: Vec<String>,
    // TODO consider having JsItems like syn::Items to enforce what is allowed at the module level
    pub stmts: Vec<JsStmt>,
}
use fmt::Write;
impl JsModule {
    pub fn js_string(&self) -> String {
        let mut temp = String::new();
        write!(&mut temp, "{}", self.stmts.fmt_join(", ")).unwrap();
        temp
    }
}
impl fmt::Display for JsModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.stmts.fmt_join("\n"))
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
    // TryBlock(Vec<JsStmt>),
    // CatchBlock(String, Vec<JsStmt>),
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
            // JsStmt::TryBlock(_) => todo!(),
            // JsStmt::CatchBlock(_, _) => todo!(),
            JsStmt::Raw(_) => todo!(),
            JsStmt::Comment(_) => todo!(),
        }
    }
    pub fn js_string(&self) -> String {
        let mut temp = String::new();
        write!(&mut temp, "{self}").unwrap();
        temp
    }
}
impl fmt::Display for JsStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsStmt::Local(local) => write!(f, "{local}"),
            JsStmt::Expr(expr, closing_semi) => {
                if *closing_semi {
                    write!(f, "{expr};")
                } else {
                    write!(f, "{expr}")
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
                                .split('.')
                                .enumerate()
                                .map(|(i, word)| {
                                    if i == 0 && path_seg.split('.').count() > 1 {
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
                write!(
                    f,
                    r#"import{}{} from "{}";"#,
                    if let Some(default) = default {
                        format!(" {default}")
                    } else {
                        "".to_string()
                    },
                    if !exports.is_empty() {
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
            JsStmt::Function(js_fn) => write!(f, "{js_fn}"),
            JsStmt::Class(js_class) => {
                // TODO name needs to be namespaced and camelcased
                // let name = match js_class.struct_or_enum {
                //     StructOrEnumSynObject::Struct(item_struct) => item_struct.ident,
                //     StructOrEnumSynObject::Enum(item_enum) => item_enum.ident,
                // };

                // Get methods
                // let js_methods = js_class.impld_methods.iter().map(|m| {
                //     match m {
                //         ImplItem::Const(_) => todo!(),
                //         ImplItem::Fn(impl_item_fn) => todo!(),
                //         ImplItem::Type(_) => todo!(),
                //         ImplItem::Macro(_) => todo!(),
                //         ImplItem::Verbatim(_) => todo!(),
                //         _ => todo!(),
                //     }
                // }).collect::<Vec<_>>();

                // format!(
                //     "class {} {{\n{}{}\n{}}}",
                //     js_class.name,
                //     // name,
                //     if !js_class.inputs.is_empty() {
                //         format!(
                //             "constructor({}) {{\n{}\n}}\n",
                //             js_class.inputs.join(", "),
                //             js_class
                //                 .inputs
                //                 .iter()
                //                 .enumerate()
                //                 .map(|(i, input)| if js_class.tuple_struct {
                //                     format!("this[{i}] = {input};")
                //                 } else {
                //                     format!("this.{input} = {input};")
                //                 })
                //                 .collect::<Vec<_>>()
                //                 .join(" "),
                //         )
                //     } else {
                //         "".to_string()
                //     },
                //     js_class
                //         .static_fields
                //         .iter()
                //         .map(|field| field.js_string())
                //         .collect::<Vec<_>>()
                //         .join("\n"),
                //     js_class
                //         .methods
                //         .iter()
                //         .map(|method| format!(
                //             "{}{}",
                //             if method.1 { "static " } else { "" },
                //             method.2.js_string()
                //         ))
                //         .collect::<Vec<_>>()
                //         .join("\n")
                // )
                write!(
                    f,
                    "class {} {{\n{}{}\n{}}}",
                    js_class.name,
                    // name,
                    if !js_class.inputs.is_empty() {
                        format!(
                            "constructor({}) {{\n{}\n}}\n",
                            js_class.inputs.fmt_join(", "),
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
                    js_class.static_fields.fmt_join("\n"),
                    js_class
                        .methods
                        .iter()
                        .map(|method| format!(
                            "{}{}",
                            if method.1 { "static " } else { "" },
                            method.2
                        ))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            JsStmt::ClassMethod(_, _, _, _) => todo!(),
            JsStmt::ClassStatic(_) => todo!(),
            JsStmt::Raw(raw_js) => write!(f, "{raw_js}"),
            JsStmt::ScopeBlock(stmts) => {
                // format!(
                //     "{{\n{}\n}}",
                //     stmts
                //         .iter()
                //         .map(|s| s.js_string())
                //         .collect::<Vec<_>>()
                //         .join("\n")
                // )
                write!(f, "{{{}\n}}", stmts.fmt_join("\n"))
            }
            JsStmt::Comment(text) => write!(f, "// {text}"),
        }
    }
}

// pub struct JsImplItemFn(ImplItemFn);
// impl fmt::Display for JsImplItemFn {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         let JsImplItemFn(impl_item_fn) = self;
//         let body_stmts = impl_item_fn.block.stmts
//     }
// }

// // pub trait SynToJs {
// //     fn write_syn_as_js(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
// // }

// /// (_, body stmts)
// pub struct JsImplItem {impl_item: ImplItem, body_stmts: Vec<JsStmt>}
// impl fmt::Display for JsImplItem {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         let JsImplItem(impl_item) = self;
//         match impl_item {
//             ImplItem::Const(_) => todo!(),
//             ImplItem::Fn(impl_item_fn) => todo!(),
//             ImplItem::Type(_) => todo!(),
//             ImplItem::Macro(_) => todo!(),
//             ImplItem::Verbatim(_) => todo!(),
//             _ => todo!(),
//         };
//         let body_stmts = self
//             .body_stmts
//             .iter()
//             // .enumerate()
//             // .map(|(i, stmt)| handle_fn_body_stmt(i, stmt, self.body_stmts.len()))
//             .map(|stmt| stmt.js_string())
//             .collect::<Vec<_>>()
//             .join("\n");
//         let fn_string = format!(
//             "{}{}{}{}({}) {{\n{}\n}}",
//             if self.export && !self.is_method {
//                 "export default "
//             } else {
//                 ""
//             },
//             if self.async_ { "async " } else { "" },
//             if self.is_method { "" } else { "function " },
//             self.name,
//             self.input_names.join(", "),
//             body_stmts
//         );
//         if self.iife {
//             format!("({})()", fn_string)
//         } else {
//             fn_string
//         }
//         write!(
//             f,
//             "TX detected: {source_owner} sent {formatted_amount} USDC to {destination_owner}"
//         )
//     }
// }

// impl  JsImplItem {
//     fn js_string(&self, impl_item_target_name: &String)  -> String {
//         let JsImplItem { impl_item, body_stmts } = self;
//         match impl_item {
//             ImplItem::Const(_) => todo!(),
//             ImplItem::Fn(impl_item_fn) => {
//                   let static_ = match impl_item_fn.sig.inputs.first() {
//                     Some(FnArg::Receiver(_)) => false,
//                     _ => true,
//                 };

//                 let input_names = impl_item_fn
//                     .clone()
//                     .sig
//                     .inputs
//                     .into_iter()
//                     .filter_map(|input| match input {
//                         FnArg::Receiver(_) => None,
//                         FnArg::Typed(pat_type) => match *pat_type.pat {
//                             Pat::Ident(pat_ident) => Some(camel(pat_ident.ident)),
//                             _ => todo!(),
//                         },
//                     })
//                     .collect::<Vec<_>>();

//                 // Get generics
//                 let mut fn_generics = impl_item_fn
//                     .sig
//                     .generics
//                     .params
//                     .iter()
//                     .map(|generic_param| match generic_param {
//                         GenericParam::Lifetime(_) => todo!(),
//                         GenericParam::Type(type_param) => {
//                             let name = type_param.ident.to_string();
//                             // let type_ = type_param
//                             //     .bounds
//                             //     .first()
//                             //     .map(|type_param_bound| {
//                             //         match get_return_type_of_type_param_bound(
//                             //             type_param_bound,
//                             //             &Vec::new(),
//                             //             current_module_path,
//                             //             &global_data,
//                             //         ) {
//                             //             RustType::ImplTrait => RustType::TypeParam(RustTypeParam {
//                             //                 name: name.clone(),
//                             //                 type_: RustTypeParamValue::Unresolved,
//                             //             }),
//                             //             RustType::TypeParam(_) => todo!(),
//                             //             RustType::Fn(return_type) => RustType::Fn(return_type),
//                             //             _ => todo!(),
//                             //         }
//                             //     })
//                             //     .unwrap_or(RustType::TypeParam(RustTypeParam {
//                             //         name: name.clone(),
//                             //         type_: RustTypeParamValue::Unresolved,
//                             //     }));
//                             RustTypeParam {
//                                 name,
//                                 type_: RustTypeParamValue::Unresolved,
//                             }
//                         }
//                         GenericParam::Const(_) => todo!(),
//                     })
//                     .collect::<Vec<_>>();

//                 // update generics with any `impl Fn... -> ...` types defined in where clauses
//                 // let where_clause = &item_impl_fn.sig.generics.where_clause;
//                 // if let Some(where_clause) = where_clause {
//                 //     for where_predicate in &where_clause.predicates {
//                 //         match where_predicate {
//                 //             WherePredicate::Lifetime(_) => todo!(),
//                 //             WherePredicate::Type(predicate_type) => {
//                 //                 let name = match &predicate_type.bounded_ty {
//                 //                     Type::Path(type_path) => {
//                 //                         type_path.path.segments.first().unwrap().ident.to_string()
//                 //                     }
//                 //                     _ => todo!(),
//                 //                 };
//                 //                 let type_param_bound = predicate_type.bounds.first().unwrap();
//                 //                 let type_ = get_return_type_of_type_param_bound(
//                 //                     type_param_bound,
//                 //                     &fn_generics,
//                 //                     current_module_path,
//                 //                     &global_data,
//                 //                 );
//                 //                 // MyGeneric { name, type_ }
//                 //                 let generic = fn_generics
//                 //                     .iter_mut()
//                 //                     .find(|my_generic| my_generic.name == name)
//                 //                     .unwrap();
//                 //                 generic.type_ = type_;
//                 //             }
//                 //             _ => todo!(),
//                 //         }
//                 //     }
//                 // }

//                 // let where_generics = item_impl_fn
//                 //     .sig
//                 //     .generics
//                 //     .where_clause
//                 //     .as_ref()
//                 //     .map(|where_clause| {
//                 //         where_clause
//                 //             .predicates
//                 //             .iter()
//                 //             .map(|where_predicate| )
//                 //             .collect::<Vec<_>>()
//                 //     })
//                 //     .unwrap_or(Vec::new());
//                 // dbg!(&where_generics);
//                 // generics.extend(where_generics);

//                 // if let Some((body_stmts, return_type)) = body_stmts {
//                 //     impl_stmts.push(
//                 //         // item_impl_fn.sig.ident.to_string(),
//                 //         ImplItemTemp {
//                 //             class_name: impl_item_target_name.clone(),
//                 //             module_path: current_module_path.clone(),
//                 //             item_stmt: JsImplItem::ClassMethod(
//                 //                 impl_item_target_name.clone(),
//                 //                 false,
//                 //                 static_,
//                 //                 JsFn {
//                 //                     iife: false,
//                 //                     public: false,
//                 //                     export: false,
//                 //                     is_method: true,
//                 //                     async_: impl_item_fn.sig.asyncness.is_some(),
//                 //                     name: camel(impl_item_fn.sig.ident.clone()),
//                 //                     input_names,
//                 //                     body_stmts,
//                 //                 },
//                 //             ),
//                 //             // return_type,
//                 //             item_name: impl_item_fn.sig.ident.to_string(),
//                 //         },
//                 //     );
//                 // } else {
//                 //     todo!();
//                 // }

//                 let js_fn = JsFn {
//                     iife: false,
//                     public: false,
//                     export: false,
//                     is_method: true,
//                     async_: impl_item_fn.sig.asyncness.is_some(),
//                     name: camel(impl_item_fn.sig.ident.clone()),
//                     input_names,
//                     body_stmts: body_stmts.clone(),
//                 };
//                 format!(
//                     "{}{}",
//                     if static_ { "static " } else { "" },
//                     js_fn.js_string()
//                 )

//             },
//             ImplItem::Type(_) => todo!(),
//             ImplItem::Macro(_) => todo!(),
//             ImplItem::Verbatim(_) => todo!(),
//             _ => todo!(),
//         }
//     }
// }
