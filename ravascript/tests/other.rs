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

#[tokio::test]
async fn testing_asserts() -> Result<(), Box<dyn std::error::Error>> {
    let js_should_not_throw = "console.assert(3 === 3, { msg: 'numbers do not match' });";
    let _ = execute_js_with_assertions(js_should_not_throw).await?;

    let js_should_throw = "console.assert(3 === 2, { msg: 'numbers do not match' });";
    assert!(execute_js_with_assertions(js_should_throw).await.is_err());
    Ok(())
}

#[tokio::test]
async fn it_executes_simple_expressions() {
    let generated_js = r2j_block_with_prelude!({
        let my_num = 5;
        assert!(my_num == 2 + 3);
        enum Colors {
            Red,
            Blue,
        }
        let blue = Colors::Blue;
        let answer = match blue {
            Colors::Red => 1,
            Colors::Blue => 2,
        };
        assert!(answer == 2);
    });

    let _ = execute_js_with_assertions(&generated_js).await.unwrap();
}

#[tokio::test]
async fn it_transpiles_vec_macro() {
    let actual = r2j_block!({
        let _data = vec![1, 2, 3];
    });
    assert_eq!(
        "var _data = [new RustInteger(1), new RustInteger(2), new RustInteger(3)];",
        actual
    );
}

#[tokio::test]
async fn it_transpiles_vec_macro2() {
    let generated_js = r2j_block_with_prelude!({
        let data = vec![1, 2, 3];
        assert!(data[1] == 2);
    });

    let _ = execute_js_with_assertions(&generated_js).await.unwrap();
}

#[tokio::test]
async fn it_transpiles_iter_map() {
    let actual = r2j_block!({
        let data = vec![1, 2, 3];
        let _data = data
            .iter()
            .map(|num| {
                let _sum = num + 2;
                num
            })
            .collect::<Vec<_>>();
    });
    let expected = r#"var data = [new RustInteger(1), new RustInteger(2), new RustInteger(3)];
var _data = data.map((num) => {
  var _sum = num.add(new RustInteger(2));
  return num;
});"#;
    // let expected = format_js(expected_js);
    assert_eq!(expected, actual);
}

#[tokio::test]
async fn array() {}
