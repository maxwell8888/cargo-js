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
use ravascript_core::{format_js, from_block, from_crate, generate_js_from_module};

// macro_rules! stmts_to_code_str {
//     ($($stmts:tt)*) => {
//         #[fn_stmts_as_str]
//         fn fn_wrapper() {
//             $($stmts)*
//         }

//     };
// }

pub fn r2j_block(code: &str) -> String {
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
#[macro_export]
macro_rules! r2j_block {
    ($block:block) => {{
        // Output the block to ensure that it runs without errors, eg failed asserts
        $block

        #[fn_stmts_as_str]
        fn fn_wrapper() $block

        r2j_block(block_code_str())
    }};
}

pub fn r2j_block_with_prelude(code: &str) -> String {
    let stmts = from_block(code, true);
    let generated_js = stmts
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n");
    let generated_js = format_js(generated_js);
    generated_js
}
/// Input code should be in a block as this allow rustfmt to work on the code, however the block (braces) are removed from the the output code and instead just the lines of code inside the block are used to generate the Javascript
#[macro_export]
macro_rules! r2j_block_with_prelude {
    ($block:block) => {{
        // Output the block to ensure that it runs without errors, eg failed asserts
        $block

        #[fn_stmts_as_str]
        fn fn_wrapper() $block

        r2j_block_with_prelude(block_code_str())
    }};
}

pub fn r2j_file(code: &str) -> String {
    let modules = from_file(code, false);
    let generated_js = modules
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n\n");
    let generated_js = format_js(generated_js);
    generated_js
}
#[macro_export]
macro_rules! r2j_file {
    ($($item:item)*) => {{
        mod generated {
            $($item)*
        }
        let file = stringify!($($item)*);
        r2j_file(file)
    }};
}

#[macro_export]
macro_rules! r2j_file_run_main {
    ($($item:item)*) => {{
        mod generated {
            $($item)*
            pub fn run_main() {
                main();
            }
        }
        generated::run_main();
        let file = stringify!($($item)*);
        r2j_file(file)
    }};
}

#[macro_export]
macro_rules! r2j_file_unchecked {
    ($($item:item)*) => {{
        let file = stringify!($($item)*);
        r2j_file(file)
    }};
}

// TODO it is nice using `:block` for `r2j_block` because then rustfmt works on it, and I believe cargo check will run on it. For files, we might want to test eg file level attributes, which shouldn't appear in a block. For 99% of cases this won't matter and want rustfmt to work, for file level attrs, cross that bridge when we come to it. Pretty sure macro_rules! inputs can be overloaded so maybe accept block *or* tt.
// Why not use stringify directly? Because we want to also output the actual Rust module so it gets checked by rustc/RA.
#[macro_export]
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

#[macro_export]
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

pub async fn exexute_js(js: &str) -> Result<bool, Box<dyn std::error::Error>> {
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

pub async fn get_rust_module_and_expected_js(
    dir_path: PathBuf,
) -> Result<(String, String), Box<dyn std::error::Error>> {
    let dir_name = dir_path.file_name().unwrap();

    let rust_file_name = format!("{}.rs", dir_name.to_string_lossy());
    let rust_file_path = dir_path.join(rust_file_name);
    let rust_input = fs::read_to_string(rust_file_path).unwrap();

    let js_file_name = format!("{}.js", dir_name.to_string_lossy());
    let js_file_path = dir_path.join(js_file_name);
    let expected_js = fs::read_to_string(js_file_path).unwrap();

    let stmts = from_file(&rust_input, true);
    let generated_js = stmts
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n");

    Ok((format_js(expected_js), format_js(generated_js)))
}

pub async fn execute_js_with_assertions(js: &str) -> Result<(), Box<dyn std::error::Error>> {
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
