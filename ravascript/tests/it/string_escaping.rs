use pretty_assertions::assert_eq;
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;

use super::utils::*;
use crate::r2j_block_with_prelude;

#[tokio::test]
async fn mutating_strings() {
    #[allow(unused_variables)]
    let actual = r2j_block_with_prelude!({
        let json = r#"{ "text": "hello", "num": 5 }"#;
        let json2 = "{ \"text\": \"hello\", \"num\": 5 }";
    });

    let expected = format_js(
        r#"
            let json = "{ \"text\": \"hello\", \"num\": 5 }";
            let json2 = "{ \"text\": \"hello\", \"num\": 5 }";
        "#,
    );

    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}
