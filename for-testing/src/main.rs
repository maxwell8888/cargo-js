mod foo_bar;
use foo_bar::External;

mod stuff;
use stuff::dog::Dog;

fn main() {
    let thing = External::new();
    let fido = Dog {
        fluffy: true,
        age: 2,
    };
}
