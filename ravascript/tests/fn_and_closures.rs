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
    let expected_js = r#"var _closure3 = (arg) => {
  var _x = arg;
};
var _closure4 = async (arg) => arg;
var _closure5 = async (arg) => {
  var _x = arg;
};
var _closure6 = async (arg) => {
  var _x = new RustString("hello");
  return arg;
}
var _closure7 = (arg) => {
  var ifTempAssignment;
  if (arg.ge(new RustInteger(0)).jsBoolean) {
    ifTempAssignment = new RustString("positive");
  } else {
    var _thing = new RustInteger(5);
    ifTempAssignment = new RustString("negative");
  }
  return ifTempAssignment;
};"#;
    let expected_js = format_js(expected_js);
    // println!("{expected_js}");
    // println!("{generated_js}");
    assert_eq!(expected_js, generated_js);

    let actual = r2j_block!({
        let _closure = |arg: i32| arg;
    });
    assert_eq!("var _closure = (arg) => arg;", actual);

    let actual = r2j_block!({
        let _closure = || {
            5;
        };
    });
    let expected = "var _closure = () => {
  new RustInteger(5);
};";
    assert_eq!(expected, actual);
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
        var _closure = (arg) => {
            var ifTempAssignment;
            if (arg.ge(new RustInteger(0)).jsBoolean) {
                var _thing = new RustInteger(5);
                ifTempAssignment = new RustString("positive");
            } else if (arg.eq(new RustInteger(0)).jsBoolean) {
                ifTempAssignment = new RustString("zero");
            } else {
                ifTempAssignment = new RustString("negative");
            }
            return ifTempAssignment;
        };
        "#,
    );
    assert_eq!(expected, actual);
}

#[tokio::test]
async fn closure_return_match() {
    let actual = r2j_block!({
        let _closure = |arg: Option<i32>| match arg {
            Some(num) => {
                let sum = num + 5;
                sum
            }
            None => 0,
        };
    });
    let expected = r#"var _closure = (arg) => {
  var ifTempAssignment;
  if (arg.id === Option.someId) {
    var [num] = arg.data;
    var sum = num.add(new RustInteger(5));
    ifTempAssignment = sum;
  } else if (arg.id === Option.noneId) {
    ifTempAssignment = new RustInteger(0);
  } else {
    throw new Error("couldn't match enum variant");
  }
  return ifTempAssignment;
};"#;
    assert_eq!(expected, actual);
}
