use ravascript::a;
use ravascript::prelude::*;

// cloning
// JSON.stringify() works for all objects except html elements, recursive objects
// structuredClone() supports more cases

fn main() {
    let cool = 5;
    let mut arr = [1, 2, 3];
    arr.push(4);
    // let a = a![1, 2, 3];
    let ss = "hi".to_upper_case();

    let woop = [1, 2, 3];
    let nice = woop.iter().map(|x| x + 1);
}
