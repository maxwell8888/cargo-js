mod foo_bar;
use foo_bar::External;

// mod colors;
// use colors::green::Green;

// mod utils;

// fn duplicate_name() -> i32 {
//     10
// }

fn main() {
    // duplicate_name();
    // utils::duplicate_name();
    let thing = External::new();
    // let fido = Green {
    //     fluffy: true,
    //     age: 2,
    // };
    // assert_eq!(fido.woof(), 32);
}
