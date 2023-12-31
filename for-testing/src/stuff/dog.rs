fn local_function() -> i32 {
    10
}

pub struct Dog {
    pub fluffy: bool,
    pub age: i32,
}
impl Dog {
    pub fn woof(&self) -> i32 {
        fn local_function() -> i32 {
            9999
        }
        self.age
            + self::local_function()
            + self::super::super::utils::say_something::say_hello()
            + crate::utils::say_something::say_hello()
            + crate::stuff::dog::local_function()
            + super::DOG_ACTIVITY
            + super::stuff_function()
    }
}
