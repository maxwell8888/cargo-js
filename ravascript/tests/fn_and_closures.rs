#![allow(unused_variables)]

mod utils;
use pretty_assertions::assert_eq;
use ravascript_core::format_js;
use ravascript_macros::fn_stmts_as_str;
use utils::*;

#[tokio::test]
async fn function_body_returns_and_async() {
    // TODO return large if else expression that must be converted to js using temp var which is then returned
    let generated_js = r2j_block!({
        let _closure3 = |arg: i32| {
            let _x = arg;
        };
        let _closure4 = |arg: i32| async move { arg };
        let _closure5 = |arg: i32| async move {
            let _x = arg;
        };
        let _closure6 = |arg: i32| async move {
            let _x = "hello";
            arg
        };
        let _closure7 = |arg: i32| {
            if arg >= 0 {
                "positive"
            } else {
                let _thing = 5;
                "negative"
            }
        };
    });
    // check code actually runs??
    // fn_code_str();

    // let generated_js = generate_js_from_block(block_code_str());
    // let generated_js = format_js(generated_js);
    // let generated_js = block_code_str();

    // let generated_js = generated_js.print().unwrap().as_code();
    let expected_js = format_js(
        r#"
        let _closure3 = (arg) => {
            let _x = arg;
        };
        let _closure4 = async (arg) => arg;
        let _closure5 = async (arg) => {
            let _x = arg;
        };
        let _closure6 = async (arg) => {
            let _x = "hello";
            return arg;
        }
        let _closure7 = (arg) => {
            var ifTempAssignment;
            if (arg >= 0) {
                ifTempAssignment = "positive";
            } else {
                let _thing = 5;
                ifTempAssignment = "negative";
            }
            return ifTempAssignment;
        };"#,
    );
    assert_eq!(expected_js, generated_js);

    let actual = r2j_block!({
        let _closure = |arg: i32| arg;
    });
    assert_eq!("let _closure = (arg) => arg;", actual);

    let actual = r2j_block!({
        let _closure = || {
            5;
        };
    });
    let expected = format_js(
        "
            let _closure = () => {
                5;
            };
        ",
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn function_returns_if_else_if_else() {
    // TODO return large if else expression that must be converted to js using temp var which is then returned
    let actual = r2j_block!({
        let _closure = |arg: i32| {
            if arg >= 0 {
                let _thing = 5;
                "positive"
            } else if arg == 0 {
                "zero"
            } else {
                "negative"
            }
        };
    });
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
            return ifTempAssignment;
        };
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn closure_return_match() {
    let actual = r2j_block!({
        enum Number {
            Some(i32),
            None,
        }
        let _closure = |arg: Number| match arg {
            Number::Some(num) => {
                let sum = num + 5;
                sum
            }
            Number::None => 0,
        };
    });
    let expected = format_js(
        r#"class Number {
            static someId = "Some";
            static noneId = "None";
            static None = new Number("None", null);
            constructor(id, data) {
              this.id = id;
              this.data = data;
            }
            static Some(arg_0) {
              return new Number("Some", [arg_0]);
            }
          }
          let _closure = (arg) => {
          var ifTempAssignment;
          if (arg.id === Number.someId) {
            var [num] = arg.data;
            let sum = num + 5;
            ifTempAssignment = sum;
          } else if (arg.id === Number.noneId) {
            ifTempAssignment = 0;
          } else {
            throw new Error("couldn't match enum variant");
          }
          return ifTempAssignment;
        };"#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}

// TODO problem with r2j_block is that I believe we are parsing it as `fn temp() { ... }` which means if the last item is an expr with no semi-colon it will add `return`
// Despite what is written in https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/block, it seems function defintions in blocks are hoisted even without adding "use strict"; and using `var`s. TODO post on stackoverflow to clarify.
#[tokio::test]
async fn hoisting_fn_def_inside_block() {
    let actual = r2j_block!({
        let num = 5;
        {
            let result = add_one(num);
            assert!(result == 6);
            fn add_one(num: i32) -> i32 {
                num + 1
            }
        }
        let todo = 2;
    });
    let expected = format_js(
        r#"
            let num = 5;
            {
                let result = addOne(num);
                console.assert(result === 6);
                function addOne(num) {
                    return num + 1;
                }
            }
            let todo = 2;
        "#,
    );
    assert_eq!(expected, actual);
    execute_js_with_assertions(&expected).await.unwrap();
}
