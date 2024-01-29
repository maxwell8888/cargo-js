mod stuff;
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
async fn it_transpiles_structs_and_impl_methods() {
    let (expected, actual) =
        get_rust_module_and_expected_js("tests/stuff/structs_and_impl_methods".into())
            .await
            .unwrap();

    let _ = execute_js_with_assertions(&expected).await.unwrap();

    assert_eq!(expected, actual);
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
        assert_eq!(cool.whatever(), 5)
    });
    let expected = r#"
    class Cool {
        whatever() {
            return new RustInteger(5);
        }
    }
    if (new RustBool(false).jsBoolean) {
        function inner() {}
    }
    var cool = new Cool();
    console.assert(cool.whatever().eq(new RustInteger(5)));
    "#;
    assert_eq!(format_js(expected), actual);
}

#[tokio::test]
async fn tuple_struct() {
    let actual = r2j_block_with_prelude!({
        struct Cool(i32);
        impl Cool {
            fn get_inner(&self) -> i32 {
                self.0
            }
            fn other_number(&self) -> i32 {
                5
            }
        }
        let cool = Cool(5);
        assert_eq!(cool.0, 5);
    });
    let expected = concat!(
        include_str!("rust_integer_prelude.js"),
        include_str!("rust_bool_prelude.js"),
        r#"class Cool {
            constructor(arg0) {
                this[0] = arg0
            }

            getInner() {
                return this[0];
            }
            otherNumber() {
                return new RustInteger(5);
            }
        }
        
        var cool = new Cool(new RustInteger(5));
        console.assert(cool[0].eq(new RustInteger(5)));
        "#
    );
    let _ = execute_js_with_assertions(&expected).await.unwrap();
    assert_eq!(format_js(expected), actual);
}
