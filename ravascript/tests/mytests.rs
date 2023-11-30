use chromiumoxide::browser::{Browser, BrowserConfig};
// use chromiumoxide_cdp::cdp::js_protocol::runtime::{
//     CallArgument, CallFunctionOnParams, EvaluateParams,
// };
use futures::StreamExt;
use prettify_js::prettyprint;
use std::{fs, path::PathBuf};
use tokio;

use ravascript::from_file;

mod stuff;

async fn get_rust_module_and_expected_js(
    dir_path: PathBuf,
) -> Result<String, Box<dyn std::error::Error>> {
    let dir_name = dir_path.file_name().unwrap();

    let rust_file_name = format!("{}.rs", dir_name.to_string_lossy());
    let rust_file_path = dir_path.join(rust_file_name);
    let rust_input = fs::read_to_string(rust_file_path).unwrap();

    let js_file_name = format!("{}.js", dir_name.to_string_lossy());
    let js_file_path = dir_path.join(js_file_name);
    let expected_js = fs::read_to_string(js_file_path).unwrap();

    let stmts = from_file(&rust_input);
    let generated_js = stmts
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n");
    let (generated_js, _) = prettyprint(generated_js.as_str());
    let (target_js, _) = prettyprint(&expected_js);

    assert_eq!(generated_js, target_js);

    Ok(target_js)
}

#[tokio::test]
async fn it_transpiles_struct_no_new() {
    let _ = get_rust_module_and_expected_js("tests/stuff/struct_no_new".into())
        .await
        .unwrap();
}

#[tokio::test]
async fn it_transpiles_structs_and_impl_methods() -> Result<(), Box<dyn std::error::Error>> {
    let target_js = get_rust_module_and_expected_js("tests/stuff/structs_and_impl_methods".into())
        .await
        .unwrap();

    let (mut browser, mut handler) = Browser::launch(BrowserConfig::builder().build()?).await?;
    let handle = tokio::task::spawn(async move {
        while let Some(h) = handler.next().await {
            match h {
                Ok(_) => continue,
                Err(_) => break,
            }
        }
    });

    let page = browser.new_page("about:blank").await?;

    let expression = format!("{target_js}\nmain();");
    let outcome: bool = page.evaluate_expression(expression).await?.into_value()?;
    println!("1 + 2 = {outcome}");
    assert!(outcome);

    browser.close().await?;
    handle.await?;
    Ok(())
}

#[tokio::test]
async fn it_transpiles_enum_match() -> Result<(), Box<dyn std::error::Error>> {
    let target_js = get_rust_module_and_expected_js("tests/stuff/enum_match".into())
        .await
        .unwrap();

    let (mut browser, mut handler) = Browser::launch(BrowserConfig::builder().build()?).await?;
    let handle = tokio::task::spawn(async move {
        while let Some(h) = handler.next().await {
            match h {
                Ok(_) => continue,
                Err(_) => break,
            }
        }
    });

    let page = browser.new_page("about:blank").await?;

    let expression = format!("{target_js}\nmain();");
    let outcome: bool = page.evaluate_expression(expression).await?.into_value()?;
    println!("1 + 2 = {outcome}");
    assert!(outcome);

    browser.close().await?;
    handle.await?;
    Ok(())
}
