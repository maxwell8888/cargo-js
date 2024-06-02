use heck::{AsKebabCase, AsPascalCase};
use syn::BinOp;

use crate::{camel, ItemDefinition, RustType};

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
    /// (base expr, field name)
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
    /// eg module::struct::associated_fn -> module.struct.associated_fn;
    Path(Vec<String>),
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
                // JS formatter converts `x => x` to `(x) => x` which seems pointless but what we will go with for now
                // let sig = if inputs.len() == 1 {
                //     inputs.get(0).unwrap().clone()
                // } else {
                //     format!("({})", inputs.join(", "))
                // };
                let sig = format!("({})", inputs.join(", "));

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
            JsExpr::TryBlock(try_block) => {
                format!(
                    "try {{\n{}\n}}",
                    try_block
                        .iter()
                        .map(|s| s.js_string())
                        .collect::<Vec<_>>()
                        .join("\n"),
                )
            }
            JsExpr::CatchBlock(err_var_name, catch_block) => {
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
        }
    }
}

#[derive(Clone, Debug)]
pub struct JsIf {
    /// The name of the initialised var we are assigning to in the final statement of the block
    pub assignment: Option<LocalName>,
    /// Whether to prepend the if statement with eg `var result = `
    /// TODO not sure why this is necessary and not handle by the original `let` `Local` statement
    pub declare_var: bool,
    pub condition: Box<JsExpr>,
    pub succeed: Vec<JsStmt>,
    /// syn has an expr as the else branch, rather than an iter of statements - because the expr might be another if expr, not always a block
    pub fail: Option<Box<JsExpr>>,
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
                                let is_error = match &**else_ {
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
    pub public: bool,
    pub export: bool,
    pub tuple_struct: bool,
    pub name: String,
    /// we are assuming input names is equivalent to field names
    pub inputs: Vec<String>,
    pub static_fields: Vec<JsLocal>,
    /// (class name, private, static, JsFn)  
    pub methods: Vec<(String, bool, bool, JsFn)>,
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
// TODO consider replacing the Vec with DestructureValue::Group, like how syn works
pub struct DestructureObject(pub Vec<DestructureValue>);
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
    pub public: bool,
    pub export: bool,
    pub type_: LocalType,
    pub lhs: LocalName,
    pub value: JsExpr,
}
impl JsLocal {
    fn js_string(&self) -> String {
        let lhs = self.lhs.js_string();
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
            JsExpr::Vanish => String::new(),
            JsExpr::Blank => format!("{var_type} {lhs};"),
            value_js_expr => format!("{var_type} {lhs} = {};", value_js_expr.js_string()),
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
    pub name: String,
    pub input_names: Vec<String>,
    pub body_stmts: Vec<JsStmt>,
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
        // JsStmt::TryBlock(_) => stmt.js_string(),
        // JsStmt::CatchBlock(_, _) => stmt.js_string(),
        JsStmt::Comment(_) => stmt.js_string(),
    }
}
impl JsFn {
    fn js_string(&self) -> String {
        // TODO private fields and methods should be prepended with `#` like `#private_method() {}` but this would require also prepending all callsites of the the field or method, which requires more sophisticated AST analysis than we currently want to do.
        let body_stmts = self
            .body_stmts
            .iter()
            // .enumerate()
            // .map(|(i, stmt)| handle_fn_body_stmt(i, stmt, self.body_stmts.len()))
            .map(|stmt| stmt.js_string())
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
    pub public: bool,
    /// camelCase JS name
    pub name: String,
    /// snake_case Rust path starting with "crate"
    pub module_path: Vec<String>,
    // TODO consider having JsItems like syn::Items to enforce what is allowed at the module level
    pub stmts: Vec<JsStmt>,
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

                format!(
                    "class {} {{\n{}{}\n{}}}",
                    js_class.name,
                    // name,
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
            JsStmt::Comment(text) => format!("// {text}"),
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
