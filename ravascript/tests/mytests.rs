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
async fn testing_asserts() -> Result<(), Box<dyn std::error::Error>> {
    let js_should_not_throw = "console.assert(3 === 3, { msg: 'numbers do not match' });";
    let _ = execute_js_with_assertions(js_should_not_throw).await?;

    let js_should_throw = "console.assert(3 === 2, { msg: 'numbers do not match' });";
    assert!(execute_js_with_assertions(js_should_throw).await.is_err());
    Ok(())
}

#[tokio::test]
async fn it_executes_simple_expressions() -> Result<(), Box<dyn std::error::Error>> {
    let generated_js = r2j_block!({
        let my_num = 5;
        Console::assert(my_num == 2 + 3);
        enum Colors {
            Red,
            Blue,
        }
        let blue = Colors::Blue;
        let answer = match blue {
            Colors::Red => 1,
            Colors::Blue => 2,
        };
        Console::assert(answer == 2);
    });
    let _ = execute_js_with_assertions(&generated_js).await?;
    Ok(())
}

#[tokio::test]
async fn it_transpiles_vec_macro() {
    let actual = r2j_block!({
        let _data = vec![1, 2, 3];
    });
    assert_eq!("var _data = [1, 2, 3];", actual);
}

#[tokio::test]
async fn it_transpiles_vec_macro2() -> Result<(), Box<dyn std::error::Error>> {
    let generated_js = r2j_block!({
        let data = vec![1, 2, 3];
        Console::assert(data[1] == 2);
    });

    let _ = execute_js_with_assertions(&generated_js).await?;
    Ok(())
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
    let expected = r#"var data = [1, 2, 3];
var _data = data.map((num) => {
  var _sum = num + 2;
  return num;
});"#;
    // let expected = format_js(expected_js);
    assert_eq!(expected, actual);
}

#[tokio::test]
async fn it_transpiles_json_parse() {
    pub struct Foo {
        bar: usize,
    }
    let actual = r2j_block!({
        fn parse(text: &str) -> Result<Foo, SyntaxError> {
            try_! {{
                return Ok(Json::parse::<Foo>(text));
            }}
            catch! {err, SyntaxError,{
                return Err(err);
            }}
        }
    });

    let expected = r#"function parse(text) {
  try {
    return Ok(JSON.parse(text));
  } catch (err) {
    return Err(err);
  }
}"#;
    assert_eq!(expected, actual);
}

#[ignore = "navigator should be lower case so not a const"]
#[tokio::test]
async fn it_writes_to_clipboard() {
    let actual = r2j_block!({
        let input = Document::create_element2::<HTMLInputElement>("input");
        let button = Document::create_element("button");
        let get_text = |_event: Event| async { NAVIGATOR.clipboard.write_text(input.value).await };
        button.add_event_listener_async("click", get_text);
    });

    let expected_js = format_js(
        r#"
        var input = document.createElement("input");
        var button = document.createElement("button");
        var getText = async (_event) => await navigator.clipboard.writeText(input.value);
        button.addEventListener("click", getText);
        "#,
    );

    assert_eq!(expected_js, actual);
}

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
  var _x = "hello";
  return arg;
}
var _closure7 = (arg) => {
  var ifTempAssignment;
  if (arg >= 0) {
    ifTempAssignment = "positive";
  } else {
    var _thing = 5;
    ifTempAssignment = "negative";
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
  5;
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
    let expected = r#"var _closure = (arg) => {
  var ifTempAssignment;
  if (arg >= 0) {
    var _thing = 5;
    ifTempAssignment = "positive";
  } else if (arg === 0) {
    ifTempAssignment = "zero";
  } else {
    ifTempAssignment = "negative";
  }
  return ifTempAssignment;
};"#;
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
  if (arg.id === someId) {
    var [num] = arg.data;
    var sum = num + 5;
    ifTempAssignment = sum;
  } else if (arg.id === noneId) {
    ifTempAssignment = 0;
  } else {
    ifTempAssignment = "this shouldn't exist";
  }
  return ifTempAssignment;
};"#;
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

#[tokio::test]
async fn mutate_int() {
    let actual = r2j_block!({
        let mut num = 0;
        num += 1;
        assert_eq!(num, 1);
    });

    let expected = r#"
    var num = 0;
    num += 1;
    console.assert(num === 1)
    "#;
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[tokio::test]
async fn mutate_int_fn_arg() {
    let actual = r2j_block!({
        let orig_num = 0;
        assert_eq!(orig_num, 0);
        fn add_one(mut num: i32) -> i32 {
            assert_eq!(num, 0);
            num += 1;
            assert_eq!(num, 1);
            num
        }
        let result = add_one(orig_num);
        assert_eq!(result, 1);
        assert_eq!(orig_num, 0);
    });

    let expected = r#"
    var origNum = 0;
    console.assert(origNum === 0);
    function addOne(num) {
        console.assert(num === 0);
        num += 1;
        console.assert(num === 1);
        return num;
    }
    var result = addOne(origNum);
    console.assert(result === 1);
    console.assert(origNum === 0);
    "#;
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

#[ignore]
#[tokio::test]
async fn mut_ref_int_fn_arg() {
    // whilst it might appear that you can mutate numbers in JS with `var x = 0; x++;`, `=`, `++`, and `+=` just reassign a value to the variable, and when we pass the variable to a function the value is copied to a new variable, and of course we are not able to reassign values to the original variable within the funciton, only the new variable.
    // A problem is that when create eg `var num = 5; var mutRef = { rustDeref: num };`, we are copying num, so if we mutate mutRef, num doesn't get updated. So I think we need ensure all mut numbers are wrapped. but how will we know how to handle `num = 4;`? Well if it is a reassignment then we know num must be mut so rhs needs wrapping? But we don't know the type of num, so it means everything gets wrapper. Whilst this seems redundant because eg an object is already mutable so doesn't need wrapping, they will actually need wrapping to support calling `.rustDeref()`. But this means if we do `num = 4; num = num; num = num;` we will be adding multiple wrappers. Rather than just wrapping, I think we need a `function mutRef(var)` which checks the type of the object and eg wraps a number or non-wrapper object, does nothing if it is already a wrapper object.
    // The problem with automatically wrapping mut numbers is that we can then no longer do `num === 0`. We can just *always* call .rustDeref() and add `Number.prototype.rustDeref = function() { return this; };`
    let actual = r2j_block!({
        let mut orig_num = 0;
        assert_eq!(orig_num, 0);

        fn add_one(num: &mut i32) -> &mut i32 {
            assert_eq!(*num, 0);
            *num += 1;
            assert_eq!(*num, 1);
            num
        }

        {
            let result = add_one(&mut orig_num);
            assert_eq!(*result, 1);
            *result += 1;
            assert_eq!(*result, 2);
        }

        assert_eq!(orig_num, 2);

        orig_num += 1;
        assert_eq!(orig_num, 3);
    });

    let expected = r#"
    Number.prototype.rustDeref = function() { return this; };
    function mutRefWrapper(value) {
        return "rustDeref" in value ? value : { rustDeref: value };
    }
    var origNum = mutRefWrapper(0);
    console.assert(origNum.rustDeref() === 0);    
    function addOne(num) {
        console.assert(num.rustDeref() === 0);
        num.rustDeref += 1;
        console.assert(num.rustDeref() === 1);
        return num;
    }
    {
        var result = addOne(mutRefWrapper(origNum));
        console.assert(result.rustDeref === 1);
        result.rustDeref += 1;
        console.assert(result.rustDeref === 2);
    }
    console.assert(origNum === 2);
    origNum += 1;
    console.assert(origNum === 3);
    "#;
    assert_eq!(format_js(expected), actual);
    let _ = execute_js_with_assertions(&expected).await.unwrap();
}

// https://www.reddit.com/r/rust/comments/3l3fgo/what_if_rust_had_mutablebox_and_immutablebox/
// Box's primary design goal is to provide a safe interface for heap allocation. Mutability is controlled by the compiler, as you've already noticed. If you want to allow mutability only to the box's contents, you can operate through a direct reference to the contents:
// let mut my_box = Box::new(11);
// // Create an &mut i32 referencing the contents of `my_box`.
// let mut my_bof_ref = &mut *my_box;
// Edit: I neglected to mention that my_box cannot be mutated or reassigned after my_box_ref is created, since mutable references are mutually exclusive, so it creates the effect you want.
#[tokio::test]
async fn mut_ref_box_contents() {}

#[tokio::test]
async fn array() {}
