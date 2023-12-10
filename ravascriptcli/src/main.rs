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
use ravascript::from_file;
// use serde::{Deserialize, Serialize};
use std::fs;

/// Ravascript  
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    // /// Filepath of the Rust file to transpile
    // #[arg(short, long)]
    // filepath: String,
    #[command(subcommand)]
    command: Commands,
}
#[derive(Subcommand, Debug)]
enum Commands {
    /// Transpile the specified js file and output to stdout
    Transpile { filepath: String },
    /// Serve a default HTML file which imports the specified js file
    Serve { filepath: Option<String> },
    // Serve(ServeArgs),
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
        Commands::Transpile { filepath } => {
            let code = fs::read_to_string(filepath).unwrap();
            let js_stmts = from_file(&code);
            let output = js_stmts
                .iter()
                .map(|stmt| stmt.js_string())
                .collect::<Vec<_>>()
                .join("\n");
            let (output, _) = prettyprint(&output);
            println!("{output}");
        }
        Commands::Serve { filepath } => {
            let app = Router::new()
                .route("/", get(root))
                .route("/index.js", get(index_js))
                .with_state(filepath.clone().unwrap_or("src/main.rs".to_string()));

            let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
            tracing::debug!("listening on {}", listener.local_addr().unwrap());
            axum::serve(listener, app).await.unwrap();
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
    let js_stmts = from_file(&code);
    let mut output = js_stmts
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>();

    output.push("main();".to_string());
    let (output, _) = prettyprint(&output.join("\n"));
    JavaScript(output)
}
