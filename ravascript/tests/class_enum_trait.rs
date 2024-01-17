mod stuff;
mod utils;
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
async fn it_transpiles_struct_no_new() {
    #[module_as_str]
    mod wrapper {
        struct MyStruct {
            my_field: i32,
        }
    }
    let expected = r#"class MyStruct {
  constructor(myField) {
    this.myField = myField;
  }
}"#;
    assert_eq!(expected, generated_js());
}

#[tokio::test]
async fn it_transpiles_structs_and_impl_methods() -> Result<(), Box<dyn std::error::Error>> {
    let (target_js, generated_js) =
        get_rust_module_and_expected_js("tests/stuff/structs_and_impl_methods".into())
            .await
            .unwrap();
    assert_eq!(target_js, generated_js);

    let outcome = exexute_js(&generated_js).await?;
    assert!(outcome);

    Ok(())
}

#[tokio::test]
async fn it_transpiles_enum_match() -> Result<(), Box<dyn std::error::Error>> {
    let (target_js, generated_js) =
        get_rust_module_and_expected_js("tests/stuff/enum_match".into())
            .await
            .unwrap();
    assert_eq!(target_js, generated_js);

    let outcome = exexute_js(&generated_js).await?;
    assert!(outcome);
    Ok(())
}

#[tokio::test]
async fn impl_in_fn_scope() {
    // impls can be inside lower *scopes* (not modules) eg inside functions (and the functions don't even need to be run)
    let actual = r2j_block!({
        struct Cool {}
        if false {
            fn inner() {
                impl Cool {
                    fn whatever(&self) -> i32 {
                        5
                    }
                }
            }
        }
        let cool = Cool {};
        cool.whatever();
    });
    let expected = r#"
    class Cool {
        whatever() {
            return 5;
        }
    }
    if (false) {
        function inner() {}
    }
    var cool = new Cool();
    cool.whatever();
    "#;
    assert_eq!(format_js(expected), actual);
}
