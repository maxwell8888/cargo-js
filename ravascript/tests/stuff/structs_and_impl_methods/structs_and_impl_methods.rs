use ravascript_core::web::Console;

trait MyTrait {
    fn get_age(&self) -> i32;
}
struct MyStruct {
    age: i32,
    name: &'static str,
}
impl MyStruct {
    fn new(age: i32, name: &'static str) -> MyStruct {
        MyStruct { age, name }
    }
    fn my_method(&self) -> &str {
        self.name
    }
    fn my_method_with_arg(&self, inc: i32) -> i32 {
        self.age + inc
    }
    fn my_associated_method(inc: i32) -> i32 {
        inc + 10
    }
    fn with_generic<T>(&self, inc: T) -> i32 {
        self.age
    }
}
// TODO currently impls for a struct must appear directly after it. defining a trait inbetween like below is not suppported
// trait MyTrait {
//     fn get_age(&self) -> i32;
// }
impl MyTrait for MyStruct {
    fn get_age(&self) -> i32 {
        self.age
    }
}

fn main() -> bool {
    let thing = MyStruct::new(2, "Bruce");
    // Console::assert(thing.my_method() == "Bruce");
    // Console::assert(thing.my_method_with_arg(2) == 4);
    // Console::assert(MyStruct::my_associated_method(2) == 12);
    // Console::assert(thing.with_generic(99) == 2);
    // Console::assert(thing.get_age() == 2);
    
    // console.assert(thing.myMethod() === "Bruce");
    // console.assert(thing.myMethodWithArg(2) === 4);
    // console.assert(MyStruct.myAssociatedMethod(2) === 12);
    // console.assert(thing.withGeneric(99) === 2);
    // console.assert(thing.getAge() === 2);
    
    let one = thing.my_method() == "Bruce";
    let two = thing.my_method_with_arg(2) == 4;
    let three = MyStruct::my_associated_method(2) == 12;
    let four = thing.with_generic(99) == 2;
    let five = thing.get_age() == 2;
    one && two && three && four && five
}
