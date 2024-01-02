use biome_formatter::{FormatLanguage, IndentStyle, IndentWidth};
use biome_js_formatter::{context::JsFormatOptions, JsFormatLanguage};
use biome_js_parser::JsParserOptions;
use biome_js_syntax::JsFileSource;
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
use ravascript_core::{
    catch, from_block, from_crate, from_fn, from_module, try_,
    web::{
        try_, Console, Document, Event, HTMLInputElement, JsError, Json, Node, SyntaxError,
        NAVIGATOR,
    },
};
// use std::sync::Arc;
// use biome_formatter::format;
// use biome_formatter::prelude::*;
use pretty_assertions::assert_eq;
use std::{fs, path::PathBuf};
use tokio;

use ravascript::from_file;
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

/// Input code should be in a block as this allow rustfmt to work on the code, however the block (braces) are removed from the the output code and instead just the lines of code inside the block are used to generate the Javascript
macro_rules! r2j_block {
    ($block:block) => {{
        #[fn_stmts_as_str]
        fn fn_wrapper() $block
        let generated_js = generate_js_from_block(block_code_str());
        let generated_js = format_js(generated_js);
        generated_js
    }};
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

fn generate_js(js: impl ToString) -> String {
    from_fn(js.to_string().as_str())
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n")
}
fn generate_js_from_block(js: impl ToString) -> String {
    from_block(js.to_string().as_str())
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n")
}
fn generate_js_from_module(js: impl ToString) -> String {
    from_module(js.to_string().as_str(), false)
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n")
}

fn format_js(js: impl ToString) -> String {
    let parse = biome_js_parser::parse_script(js.to_string().as_str(), JsParserOptions::default());
    let stmt = parse.syntax().children().nth(1).unwrap();
    let opts = JsFormatOptions::new(JsFileSource::default())
        // .with_indent_width(IndentWidth::from(1))
        .with_indent_style(IndentStyle::Space);
    let formatted_js = biome_formatter::format_node(&stmt, JsFormatLanguage::new(opts)).unwrap();
    formatted_js.print().unwrap().as_code().to_string()
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
async fn it_transpiles_crate_directory() {
    let actual = from_crate("../for-testing".into(), false);
    let actual = actual
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n");

    let expected = r#"
    var crate = {
        fooBar: {
            Internal: class Internal {
                constructor(age) {
                    this.age = age;
                }
    
                addTen() {
                    return this.age + 10
                }
            },
            External: class External {
                constructor(sub, count) {
                    this.sub = sub;
                    this.count = count;
                }
    
                static new() {
                    return new External(new Internal(0), 9);
                }
            }
        },
        stuff: {
            dogActivity: 10,
            dog: {
                localFunction: function localFunction() {
                    return 10;
                },
                Dog: class Dog {
                    constructor(fluffy, age) {
                        this.fluffy = fluffy;
                        this.age = age;
                    }

                    woof() {
                        function localFunction() {
                            return 9999;
                        }
                        return (
                            this.age +
                            this.localFunction() +
                            this.super.super.utils.saySomething.sayHello() +
                            crate.utils.saySomething.sayHello() +
                            crate.stuff.dog.localFunction() +
                            super.dogActivity +
                            super.stuffFunction()
                        )
                    }
                }
            },
            stuffFunction: function stuffFunction() {
              return 10;
            },
        },
        utils: {
            saySomething: {
                sayHello: function sayHello() {
                    return 10;
                }    
            }
        },
        main: function main() {
            var thing = External.new();
            var fido = new Dog(true, 2);
            assert.strictEqual(fido.woof(), 32);
        },
        init: function init() {
            // crate
            this.fooBar.crate = this;
            this.stuff.crate = this;
            this.stuff.dog.crate = this;
            this.utils.crate = this;
            this.utils.saySomething.crate = this;
            
            // super
            this.fooBar.super = this;
            this.stuff.super = this;
            this.stuff.dog.super = this.stuff;
            this.utils.super = this;
            this.utils.saySomething.super = this.utils;
            
            // use
            this.External = this.fooBar.External;
            this.Dog = this.stuff.dog.Dog;
            
            delete this.init;
            return this;
          },
    }.init();
    main();
    "#;

    // println!("{}", expected);
    // println!("{}", format_js(expected));
    // println!("{}", &actual);
    // println!("{}", format_js(&actual));

    assert_eq!(format_js(""), format_js(actual));
}
