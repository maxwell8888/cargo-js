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

#[ignore]
#[tokio::test]
async fn if_let_some() {
    let actual = r2j_block_with_prelude!({
        let mut counter = 0;
        let some_num = Some(5);
        if let Some(num) = some_num {
            counter += num;
        } else {
            counter += 1;
        }
        assert_eq!(counter, 5);
        if let Some(num) = None::<i32> {
            counter += num;
        } else {
            counter += 1;
        }
        assert_eq!(counter, 6);
    });

    let expected = concat!(
        include_str!("option_prelude.js"),
        "var Some = Option.Some;",
        "var None = Option.None;",
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"var counter = 0;
        var someNum = Some(5);
        if (someNum.id === Option.someId) {
            var [num] = someNum.data;
            counter += num;
        } else {
            counter += 1;
        }
        console.assert(counter.eq(5));
        if (None.id === Option.someId) {
            var [num] = someNum.data;
            counter += num;
        } else {
            counter += 1;
        }
        console.assert(counter.eq(6));
        "#
    );

    // assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}
