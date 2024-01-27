enum MyEnum {
    FooBar,
    // Foo(MyType),
    Bar { x: i32, y: &'static str },
    Baz(&'static str, i32),
}
fn main() -> bool {
    let my_data = MyEnum::FooBar;
    let my_data = MyEnum::Bar { x: 4, y: "Hello" };
    let my_data = MyEnum::Baz("Hi", 5);
    // TODO need to use a better pretty printer cos the current one messes up the the destructure formatting
    let match_result = match my_data {
        MyEnum::FooBar => 1,
        MyEnum::Bar { x, y } => {
            Console::log(x);
            Console::log(y);
            x
        }
        MyEnum::Baz(text, num) => {
            Console::log(text);
            Console::log(num);
            num
        }
    };
    assert_eq!(match_result, 5);
}
