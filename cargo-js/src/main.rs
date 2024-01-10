use axum::{
    extract::State,
    http::StatusCode,
    response::{Html, IntoResponse},
    routing::{get, post},
    Json, Router,
};
use axum_extra::response::JavaScript;
use clap::{Args, Parser, Subcommand};
use prettify_js::prettyprint;
use ravascript::{format_js, from_crate, from_file};
// use serde::{Deserialize, Serialize};
use std::{fs, path::PathBuf, env};

/// Ravascript  
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[clap(name = "JS")]
    js: Option<String>,

    // /// Filepath of the Rust file to transpile
    // #[arg(short, long)]
    // filepath: String,
    #[command(subcommand)]
    command: Commands,
}
#[derive(Subcommand, Debug)]
enum Commands {
    /// Transpile the specified js file and output to stdout
    Transpile {
        #[arg(short, long)]
        release: bool,

        #[arg(short, long)]
        package: Option<String>,

        #[arg(short, long)]
        filepath: Option<String>,

        #[arg(short, long)]
        out: Option<String>,
    },
    /// Serve a default HTML file which imports the specified js file
    Serve {
        #[arg(short, long)]
        release: bool,

        #[arg(short, long)]
        package: Option<String>,

        #[arg(short, long)]
        filepath: Option<String>,
    },
    Run {
        #[arg(short, long)]
        release: bool,

        #[arg(short, long)]
        package: Option<String>,

        #[arg(short, long)]
        filepath: Option<String>,
    }, // Serve(ServeArgs),
}
// #[derive(Args, Debug)]
// struct ServeArgs {
//     filepath: String,
// }

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt::init();

    let cli = Cli::parse();

    match &cli.command {
        Commands::Transpile {
            release,
            package,
            filepath,
            out,
        } => {
            // TODO consider using match to check that both filepath and cratepath haven't been set
            let js_str = if let Some(filepath) = filepath {
                let code = fs::read_to_string(filepath).unwrap();
                let js_stmts = from_file(&code, false);
                js_stmts
                    .iter()
                    .map(|stmt| stmt.js_string())
                    .collect::<Vec<_>>()
                    .join("\n")
            } else if let Some(crate_path) = package {
                let modules = from_crate(crate_path.into(), true);
                modules
                    .iter()
                    .map(|module| module.js_string())
                    .collect::<Vec<_>>()
                    .join("\n\n")
            } else {
                // Look for src/main.rs or src/lib.rs in current folder
                let crate_path = if PathBuf::from("src/main.rs").exists() {
                    "src/main".into()
                } else if PathBuf::from("src/lib.rs").exists() {
                    "src/lib.rs".into()
                } else {
                    eprintln!("Could not find src/main.rs or src/lib.rs in current directory");
                    std::process::exit(1);
                };
                let modules = from_crate(crate_path, true);
                modules
                    .iter()
                    .map(|module| module.js_string())
                    .collect::<Vec<_>>()
                    .join("\n\n")
            };
            let output = format_js(js_str);
            if let Some(out) = out {
                let _ = fs::write(out, output).unwrap();
            } else {
                println!("{output}");
            }
        }
        Commands::Serve {
            release,
            filepath,
            package,
        } => {
            let app = Router::new()
                .route("/", get(root))
                .route("/index.js", get(index_js))
                .with_state(filepath.clone().unwrap_or("src/main.rs".to_string()));

            let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
            tracing::debug!("listening on {}", listener.local_addr().unwrap());
            axum::serve(listener, app).await.unwrap();
        }
        Commands::Run {
            release,
            filepath,
            package,
        } => {
            // TODO run with Deno?
        }
    }
}

// basic handler that responds with a static string
async fn root() -> impl IntoResponse {
    let html = r#"
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <title>Ravascript</title>
    </head>
    <body>
        <script src="/index.js"></script>
    </body>
</html>
    "#;
    Html(html)
}
// basic handler that responds with a static string
async fn index_js(State(filepath): State<String>) -> impl IntoResponse {
    let code = fs::read_to_string(filepath).unwrap();
    let js_stmts = from_file(&code, true);
    let mut output = js_stmts
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>();

    output.push("main();".to_string());
    let (output, _) = prettyprint(&output.join("\n"));
    JavaScript(output)
}
