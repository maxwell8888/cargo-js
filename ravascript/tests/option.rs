mod utils;
use pretty_assertions::{assert_eq, assert_ne};
use ravascript::prelude::web::{
    try_, Console, Document, Event, HTMLInputElement, JsError, Json, Node, SyntaxError, NAVIGATOR,
};
use ravascript::prelude::*;
use ravascript::{catch, try_};
use ravascript_core::{format_js, from_block, from_crate, generate_js_from_module};
use ravascript_macros::module_as_str;
use ravascript_macros::{fn_as_str, fn_stmts_as_str};
use utils::*;

// TODO avoid rename if same var name is used for condition and some value
// TODO if condition is an expression (and the Some() inner is actually used) it should be evaluated and assigned to a var before being used
#[tokio::test]
async fn option_match() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        let result = match &five {
            Some(val) => *val,
            None => 0,
        };
        assert!(result == 5);
    });

    let expected = format_js(
        r#"
            let five = 5;
            let result = (() => {
                if (five !== null) {
                    let val = five;
                    return val;
                } else {
                    return 0;
                }
            })();
            console.assert(result === 5);
        "#,
    );

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn option_is_some_and() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        // let not_five: Option<i32> = None;
        // assert!(five.is_some_and(|x| x == 5));
        // assert!(!not_five.is_some_and(|x| x == 5));
    });

    let expected = format_js(
        r#"
            Object.prototype.isSomeAnd = (f) => this !== null ? f(this) : false;
            let five = 5;
            let notFive = null;
            console.assert(five.isSomeAnd((x) => x === 5));
            console.assert(!notFive.isSomeAnd((x) => x === 5));
        "#,
    );

    assert_eq!(expected, actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore]
#[tokio::test]
async fn option_unwrap() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        assert_eq!(five.unwrap(), 5);
    });

    let expected = format_js(concat!(
        include_str!("option_prelude.js"),
        "let Some = Option.Some;",
        r#"let five = Some(5);
        console.assert(five.unwrap().eq(5));
        "#,
    ));

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}

#[ignore]
#[tokio::test]
async fn option_unwrap_or() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        assert_eq!(five.unwrap_or(0), 5);
        let nothing = None;
        assert_eq!(nothing.unwrap_or(0), 0);
    });

    let expected = format_js(concat!(
        include_str!("option_prelude.js"),
        "let Some = Option.Some;
        let None = Option.None;",
        "let five = Some(5);
        console.assert(five.unwrapOr(0).eq(5));
        let nothing = None;
        console.assert(nothing.unwrapOr(0).eq(0));
        ",
    ));

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}

#[ignore]
#[tokio::test]
async fn option_unwrap_or_else() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        let none: Option<i32> = None;
        assert_eq!(five.unwrap_or_else(|| 4), 5);
        assert_eq!(none.unwrap_or_else(|| 4), 4);
    });

    let expected = format_js(concat!(
        include_str!("option_prelude.js"),
        "let Some = Option.Some;
        let None = Option.None;",
        "let five = Some(5);
        let none = None;
        console.assert(five.unwrapOrElse(() => 4).eq(5));
        console.assert(none.unwrapOrElse(() => 4).eq(4));
        ",
    ));

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}

// TODO unwrap_or_default uses T::default() which means we would need to know the type of five or none so we can convert it to eg i32.default() ???
#[ignore = "needs to know type of instance"]
#[tokio::test]
async fn option_unwrap_or_default() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        let none: Option<i32> = None;
        assert_eq!(five.unwrap_or_default(), 5);
        assert_eq!(none.unwrap_or_default(), 0);
    });

    let expected = format_js(concat!(
        include_str!("option_prelude.js"),
        "let Some = Option.Some;
        let None = Option.None;",
        r#"let five = Some(5);
        let none = None;
        console.assert(five.unwrapOrElse(() => 4).eq(5));
        console.assert(none.unwrapOrElse(() => 4).eq(4));
        "#,
    ));

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}

#[ignore]
#[tokio::test]
async fn option_map() {
    let actual = r2j_block_with_prelude!({
        let five = Some(5);
        let none: Option<i32> = None;
        assert!(five.map(|x| x + 2) == Some(7));
        assert!(none.map(|x| x + 2) == None);
    });
    // let expected = format_js(concat!(
    //     include_str!("option_prelude.js"),
    //     "let Some = Option.Some;",
    //     "let None = Option.None;",
    //     r#"let five = Some(5);
    //     let none = None;
    //     console.assert(five.map((x) => x.add(2)).eq(Some(7)));
    //     console.assert(none.map((x) => x.add(2)).eq(None));
    //     let _ = 5.eq(4);
    //     "#,
    // ));
    let expected = format_js(
        r#"
            Object.prototype.map = function(f) {
                if this !== null {
                    return f(this);
                } else {
                    return null;
                }
            };
            let five = 5;
            let none = null;
            var result;
            console.assert(five.map((x) x + 2) === 7)
            console.assert(none.map((x) x + 2) === null)
        "#,
    );

    // let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
}
