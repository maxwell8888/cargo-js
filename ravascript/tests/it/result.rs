use pretty_assertions::assert_eq;
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;

use super::utils::*;
use crate::r2j_block_with_prelude;

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
    execute_js_with_assertions(&expected).await.unwrap();
}
