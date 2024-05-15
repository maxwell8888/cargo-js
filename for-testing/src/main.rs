mod foo_bar;
use foo_bar::External;

mod colors;
use colors::green::Green;

mod utils;

mod crate_level_inline {
    pub fn duplicate_name() -> i32 {
        1
    }
    pub mod crate_level_inline_sub {
        pub fn duplicate_name() -> i32 {
            11
        }
    }
}

fn duplicate_name() -> i32 {
    10
}

fn main() {
    duplicate_name();
    utils::duplicate_name();
    let thing = External::new();
    let fido = Green {
        fluffy: true,
        age: 2,
    };
    fido.woof();
}
