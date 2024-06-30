mod utils;
use std::panic::catch_unwind;

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

#[ignore = "TODO"]
#[tokio::test]
async fn simple_result() {
    let actual = r2j_block_with_prelude!({
        let five: Result<i32, &str> = Ok(5);
        let result = match &five {
            Ok(val) => *val,
            Err(_) => 0,
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
