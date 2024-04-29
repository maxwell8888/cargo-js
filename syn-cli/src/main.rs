use clap::Parser;
use syn::{parse_str, Expr, Stmt};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    code: String,
}

fn main() {
    let args = Args::parse();
    let code = &args.code;
    if let Ok(expr) = parse_str::<Expr>(code) {
        println!("{:#?}", expr);
    } else if let Ok(stmt) = parse_str::<Stmt>(code) {
        println!("{:#?}", stmt);
    } else {
        eprintln!("could not parse");
    }
}
