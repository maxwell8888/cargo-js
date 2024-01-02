mod foo_bar;
use foo_bar::External;

mod colors;
use colors::green::Dog;

mod utils;

fn duplicate_name() -> i32 {
    10
}

fn main() {
    duplicate_name();
    utils::duplicate_name();
    let thing = External::new();
    let fido = Dog {
        fluffy: true,
        age: 2,
    };
    assert_eq!(fido.woof(), 32);
}
