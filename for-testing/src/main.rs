mod foo_bar;
use foo_bar::External;

mod stuff;
use stuff::dog::Dog;

mod utils;

fn main() {
    let thing = External::new();
    let fido = Dog {
        fluffy: true,
        age: 2,
    };
    assert_eq!(fido.woof(), 32);
}
