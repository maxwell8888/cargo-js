use clap::Parser;
use prettify_js::prettyprint;
use ravascript::from_file;
use std::fs;

/// Ravascript
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Filepath of the Rust file to transpile
    #[arg(short, long)]
    filepath: String,
}

fn main() {
    let args = Args::parse();

    let code = fs::read_to_string(args.filepath).unwrap();
    let js_stmts = from_file(&code);
    let output = js_stmts
        .iter()
        .map(|stmt| stmt.js_string())
        .collect::<Vec<_>>()
        .join("\n");
    let (output, _) = prettyprint(&output);
    println!("{output}");
}
