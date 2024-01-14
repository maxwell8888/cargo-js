use chromiumoxide::{
    browser::{Browser, BrowserConfig},
    cdp::js_protocol::debugger::{
        EventPaused, PausedReason, ResumeParams, SetPauseOnExceptionsParams,
        SetPauseOnExceptionsState,
    },
    Page,
};
// use chromiumoxide_cdp::cdp::js_protocol::runtime::{
//     CallArgument, CallFunctionOnParams, EvaluateParams,
// };
use anyhow::{anyhow, Context, Result};
// use chromiumoxide::cdp::js_protocol::debugger::*;
use futures::StreamExt;
use prettify_js::prettyprint;
// use std::sync::Arc;
// use biome_formatter::format;
// use biome_formatter::prelude::*;
use pretty_assertions::assert_eq;
use std::{fs, path::PathBuf};
use tokio;

use ravascript::from_file;
use ravascript::prelude::web::{
    try_, Console, Document, Event, HTMLInputElement, JsError, Json, Node, SyntaxError, NAVIGATOR,
};
use ravascript::prelude::*;
use ravascript::{catch, try_};
use ravascript_core::{format_js, from_block, from_crate, generate_js_from_module};
use ravascript_macros::module_as_str;
use ravascript_macros::{fn_as_str, fn_stmts_as_str};

mod stuff;

// macro_rules! stmts_to_code_str {
//     ($($stmts:tt)*) => {
//         #[fn_stmts_as_str]
//         fn fn_wrapper() {
//             $($stmts)*
//         }

//     };
// }

fn r2j_block(code: &str) -> String {
    let stmts = from_block(code, false);
    let generated_js = stmts
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n");
    let generated_js = format_js(generated_js);
    generated_js
}
/// Input code should be in a block as this allow rustfmt to work on the code, however the block (braces) are removed from the the output code and instead just the lines of code inside the block are used to generate the Javascript
macro_rules! r2j_block {
    ($block:block) => {{
        // Output the block to ensure that it runs without errors, eg failed asserts
        $block

        #[fn_stmts_as_str]
        fn fn_wrapper() $block

        r2j_block(block_code_str())
    }};
}

fn r2j_file(code: &str) -> String {
    let modules = from_file(code, false);
    let generated_js = modules
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n\n");
    let generated_js = format_js(generated_js);
    generated_js
}
macro_rules! r2j_file {
    ($($item:item)*) => {{
        mod generated {
            $($item)*
        }
        let file = stringify!($($item)*);
        r2j_file(file)
    }};
}

macro_rules! r2j_file_unchecked {
    ($($item:item)*) => {{
        let file = stringify!($($item)*);
        r2j_file(file)
    }};
}

// TODO it is nice using `:block` for `r2j_block` because then rustfmt works on it, and I believe cargo check will run on it. For files, we might want to test eg file level attributes, which shouldn't appear in a block. For 99% of cases this won't matter and want rustfmt to work, for file level attrs, cross that bridge when we come to it. Pretty sure macro_rules! inputs can be overloaded so maybe accept block *or* tt.
// Why not use stringify directly? Because we want to also output the actual Rust module so it gets checked by rustc/RA.
macro_rules! r2j_module {
    // rustfmt tt seems to work fine for tt, so I can't remember what the advantage of a block over tt was. For now stick with item because it doesn't require {}, though does require wrapping in `mod foo {}` but that makes it clear we are defining a module and dissallows incorrect code (except for using other items like fn instead)
    // ($block:tt) => {{
    ($module:item) => {
        {
            $module
            let file = stringify!($module);
            r2j_file(file)
        }
    };
}

macro_rules! r2j_assert_eq {
    ($block:block, $expected:literal) => {
        assert_eq!($expected, {
            #[fn_stmts_as_str]
            fn fn_wrapper() $block
            let generated_js = generate_js_from_block(block_code_str());
            let generated_js = format_js(generated_js);
            generated_js
        });
    };
}

async fn exexute_js(js: &str) -> Result<bool, Box<dyn std::error::Error>> {
    let (mut browser, mut handler) = Browser::launch(BrowserConfig::builder().build()?).await?;
    let handle = tokio::task::spawn(async move {
        while let Some(h) = handler.next().await {
            match h {
                Ok(_) => continue,
                Err(_) => break,
            }
        }
    });
    let expression = format!("{js}");
    let page = browser.new_page("about:blank").await?;
    let outcome: bool = page.evaluate_expression(expression).await?.into_value()?;
    browser.close().await?;
    handle.await?;
    Ok(outcome)
}

async fn get_rust_module_and_expected_js(
    dir_path: PathBuf,
) -> Result<(String, String), Box<dyn std::error::Error>> {
    let dir_name = dir_path.file_name().unwrap();

    let rust_file_name = format!("{}.rs", dir_name.to_string_lossy());
    let rust_file_path = dir_path.join(rust_file_name);
    let rust_input = fs::read_to_string(rust_file_path).unwrap();

    let js_file_name = format!("{}.js", dir_name.to_string_lossy());
    let js_file_path = dir_path.join(js_file_name);
    let expected_js = fs::read_to_string(js_file_path).unwrap();

    let stmts = from_file(&rust_input, false);
    let generated_js = stmts
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n");
    let (generated_js, _) = prettyprint(generated_js.as_str());
    let (target_js, _) = prettyprint(&expected_js);

    Ok((target_js, generated_js))
}

async fn execute_js_with_assertions(js: &str) -> Result<(), Box<dyn std::error::Error>> {
    let (mut browser, mut handler) =
        Browser::launch(BrowserConfig::builder().build().map_err(|e| anyhow!(e))?)
            .await
            .context("Failed to launch browser")?;
    let handle = tokio::task::spawn(async move {
        while let Some(h) = handler.next().await {
            match h {
                Ok(_) => continue,
                Err(_) => break,
            }
        }
    });
    let page = browser
        .new_page("about:blank")
        .await
        .context("Failed to create new page")?;
    page.enable_debugger()
        .await
        .context("Failed to enable debugger")?;

    let mut events = page
        .event_listener::<EventPaused>()
        .await
        .context("Failed to create event listener")?;
    let events_handle = tokio::spawn({
        let page = page.clone();
        async move {
            let mut fail = false;
            while let Some(event) = events.next().await {
                if event.reason == PausedReason::Assert {
                    fail = true
                }

                page.execute(ResumeParams::default())
                    .await
                    .context("Failed to execute ResumeParams")?;
            }

            if fail {
                Err(anyhow!("Assertion false"))
            } else {
                Ok(())
            }
        }
    });

    page.execute(SetPauseOnExceptionsParams {
        state: SetPauseOnExceptionsState::All,
    })
    .await
    .context("Failed to set pause on exceptions")?;
    page.evaluate_expression(js)
        .await
        .context("Failed to evaluate expression")?;

    browser.close().await.context("Failed to close browser")?;
    handle.await.context("Failed in handle.await")?;
    let event_result = events_handle
        .await
        .context("Failed in events_handle.await")??;

    Ok(())
}

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

// #[ignore]
#[tokio::test]
async fn it_transpiles_crate_directory() {
    let actual = from_crate("../for-testing".into(), false, false);

    let expected = r#"
    // crate
    function duplicateName() {
      return 10;
    }
    (function main() {
      duplicateName();
      utils__colors__duplicateName();
      var thing = External.new();
      var fido = new Green(true, 2);
      console.assert(fido.woof() === 32);
    })();
    
    // foo_bar
    class Internal {
      constructor(age) {
        this.age = age;
      }
    
      addTen() {
        return this.age + 10;
      }
    }
    class External {
      constructor(sub, count) {
        this.sub = sub;
        this.count = count;
      }
    
      static new() {
        return new External(new Internal(0), 9);
      }
    }
    
    // colors
    var DOG_ACTIVITY = 10;
    function colors__duplicateName() {
      return 10;
    }
    function stuffFunction() {
      return 10;
    }
    
    // colors::green
    function green__duplicateName() {
      return 3;
    }
    class Green {
      constructor(fluffy, age) {
        this.fluffy = fluffy;
        this.age = age;
      }
    
      woof() {
        function green__duplicateName() {
          return 9999;
        }
        return (
          this.age +
          green__duplicateName() +
          green__duplicateName() +
          sayHello() + 
          duplicateName() +
          sayHello() +
          green__duplicateName() +
          DOG_ACTIVITY +
          stuffFunction()
        );
      }
    }
    
    // utils
    
    // utils::say_something
    function sayHello() {
      return 10;
    }
    
    // utils::colors
    function utils__colors__duplicateName() {
      return 10;
    }
    "#;

    // println!("{}", expected);
    // println!("{}", format_js(expected));
    // println!("{}", &actual);
    // println!("{}", format_js(&actual));

    assert_eq!(format_js(expected), format_js(actual));
}

// TODO it might be better to rely on the for-testing dir for testing `crate` rather than using unchecked Rust
#[tokio::test]
async fn simple_module() {
    // let actual = r2j_file_unchecked!(
    // TODO I think I would actually prefer to have an explicit `mod wrapper { }` in cases like this. Whilst it is more verbose, it makes it much clearer what `self` if referring to.
    let actual = r2j_file!(
        struct Bar {}
        pub fn baz() {
            let _ = Bar {};
        }
        mod foo {
            fn green() {
                // let blue = crate::baz();
            }
        }
    );
    let expected = r#"// crate
class Bar {}
function baz() {
  var _ = new Bar();
}

// foo
function green() {}"#;
    assert_eq!(expected, actual);
}

#[tokio::test]
async fn module_super() {
    // let actual = r2j_file_unchecked!(
    // TODO I think I would actually prefer to have an explicit `mod wrapper { }` in cases like this. Whilst it is more verbose, it makes it much clearer what `self` if referring to.
    let actual = r2j_file!(
        struct Bar {}
        fn baz() {
            let _ = Bar {};
        }
        mod foo {
            fn green() {
                let blue = super::baz();
            }
        }
    );
    let expected = r#"// crate
class Bar {}
function baz() {
  var _ = new Bar();
}

// foo
function green() {
  var blue = baz();
}"#;
    assert_eq!(expected, actual);
}

#[tokio::test]
async fn module_self() {
    let actual = r2j_file!(
        struct Bar {}
        fn baz() {
            let _ = Bar {};
        }
        fn green() {
            let blue = self::baz();
        }
    );
    let expected = r#"class Bar {}
function baz() {
  var _ = new Bar();
}
function green() {
  var blue = baz();
}"#;
    assert_eq!(expected, actual);
}

#[tokio::test]
async fn module_crate() {
    let actual = r2j_file_unchecked!(
        fn baz() -> i32 {
            5
        }
        mod foo {
            fn green() {
                let blue = crate::baz();
            }
        }
    );
    let expected = r#"// crate
function baz() {
  return 5;
}

// foo
function green() {
  var blue = baz();
}"#;
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
