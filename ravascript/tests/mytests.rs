use biome_formatter::FormatLanguage;
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
use ravascript_core::{from_fn, web::Console};
// use std::sync::Arc;
// use biome_formatter::format;
// use biome_formatter::prelude::*;
use std::{fs, path::PathBuf};
use tokio;

use ravascript::from_file;
use ravascript_macros::fn_as_str;

mod stuff;

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
    let expression = format!("{js}\nmain();");
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

    Ok((generated_js, target_js))
}

#[tokio::test]
async fn it_transpiles_struct_no_new() {
    let (generated_js, target_js) =
        get_rust_module_and_expected_js("tests/stuff/struct_no_new".into())
            .await
            .unwrap();
    assert_eq!(generated_js, target_js);
}

#[tokio::test]
async fn it_transpiles_structs_and_impl_methods() -> Result<(), Box<dyn std::error::Error>> {
    let (generated_js, target_js) =
        get_rust_module_and_expected_js("tests/stuff/structs_and_impl_methods".into())
            .await
            .unwrap();
    assert_eq!(generated_js, target_js);

    let outcome = exexute_js(&generated_js).await?;
    assert!(outcome);

    Ok(())
}

#[tokio::test]
async fn it_transpiles_enum_match() -> Result<(), Box<dyn std::error::Error>> {
    let (generated_js, target_js) =
        get_rust_module_and_expected_js("tests/stuff/enum_match".into())
            .await
            .unwrap();
    assert_eq!(generated_js, target_js);

    let outcome = exexute_js(&generated_js).await?;
    dbg!(outcome);
    assert!(outcome);
    Ok(())
}

#[tokio::test]
async fn it_transpiles_simple() -> Result<(), Box<dyn std::error::Error>> {
    #[fn_as_str]
    fn jsfn() -> bool {
        let cool = 5;
        cool == 5
    }
    let generated_js = from_fn(jsfn_code_str())
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n");
    // dbg!(&generated_js);
    // assert_eq!(generated_js, target_js);

    let (mut browser, mut handler) = Browser::launch(BrowserConfig::builder().build()?).await?;
    let handle = tokio::task::spawn(async move {
        while let Some(h) = handler.next().await {
            match h {
                Ok(_) => continue,
                Err(_) => break,
            }
        }
    });
    dbg!(jsfn_code_str());

    let page = browser.new_page("about:blank").await?;

    let expression = format!("{}\njsfn();", jsfn_code_str());
    let outcome: bool = page.evaluate_expression(generated_js).await?.into_value()?;
    println!("1 + 2 = {outcome}");
    assert!(outcome);

    browser.close().await?;
    handle.await?;
    Ok(())
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
async fn testing_asserts() -> Result<(), Box<dyn std::error::Error>> {
    let js_should_not_throw = "console.assert(3 === 3, { msg: 'numbers do not match' });";
    let _ = execute_js_with_assertions(js_should_not_throw).await?;

    let js_should_throw = "console.assert(3 === 2, { msg: 'numbers do not match' });";
    assert!(execute_js_with_assertions(js_should_throw).await.is_err());
    Ok(())
}

#[tokio::test]
async fn it_transpiles_simple2() -> Result<(), Box<dyn std::error::Error>> {
    #[fn_as_str]
    fn jsfn() {
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
    }

    // Test the Rust code
    jsfn();
    let generated_js = from_fn(jsfn_code_str())
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n");
    fs::write("deleteme.js", &generated_js).unwrap();
    // println!("{generated_js}");

    // Test the generated JS code
    let _ = execute_js_with_assertions(&generated_js).await?;
    Ok(())
}

#[tokio::test]
async fn it_transpiles_vec_macro() {
    #[fn_as_str]
    fn jsfn() {
        let data = vec![1, 2, 3];
    }

    jsfn();
    let generated_js = from_fn(jsfn_code_str())
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n");
    assert_eq!(generated_js, "var data = [1, 2, 3];");
}

#[tokio::test]
async fn it_transpiles_vec_macro2() -> Result<(), Box<dyn std::error::Error>> {
    #[fn_as_str]
    fn jsfn() {
        let data = vec![1, 2, 3];
        Console::assert(data[1] == 2);
    }

    jsfn();
    let generated_js = from_fn(jsfn_code_str())
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n");
    let _ = execute_js_with_assertions(&generated_js).await?;
    Ok(())
}

#[tokio::test]
async fn it_transpiles_iter_map() {
    #[fn_as_str]
    fn jsfn() {
        let data = vec![1, 2, 3];
        let data = data
            .iter()
            .map(|num| {
                let sum = num + 2;
                num
            })
            .collect::<Vec<_>>();
    }

    jsfn();

    let generated_js = generate_js(jsfn_code_str());
    let generated_js = format_js(generated_js);

    // let generated_js = generated_js.print().unwrap().as_code();
    let expected_js = r#"var data = [1, 2, 3];
var data = data.map(num => {
    var sum = num + 2;
    return num;
});"#;
    let expected_js = format_js(expected_js);
    println!("{expected_js}");
    assert_eq!(generated_js, expected_js);
}

fn generate_js(js: impl ToString) -> String {
    from_fn(js.to_string().as_str())
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n")
}

fn format_js(js: impl ToString) -> String {
    let parse = biome_js_parser::parse_script(js.to_string().as_str(), JsParserOptions::default());
    let stmt = parse.syntax().children().nth(1).unwrap();
    let formatted_js = biome_formatter::format_node(
        &stmt,
        JsFormatLanguage::new(JsFormatOptions::new(JsFileSource::default())),
    )
    .unwrap();
    formatted_js.print().unwrap().as_code().to_string()
}
