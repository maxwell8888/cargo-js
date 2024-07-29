use pretty_assertions::assert_eq;
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;

use super::utils::*;
use crate::r2j_block;

#[ignore = "reason"]
#[tokio::test]
async fn option() {
    let actual = r2j_block!({});
    let expected = format_js(
        r#"
        let _closure = (arg) => {
            var ifTempAssignment;
            if (arg >= 0) {
                let _thing = 5;
                ifTempAssignment = "positive";
            } else if (arg === 0) {
                ifTempAssignment = "zero";
            } else {
                ifTempAssignment = "negative";
            }
k            return ifTempAssignment;
        };
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}
