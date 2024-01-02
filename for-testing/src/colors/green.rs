fn duplicate_name() -> i32 {
    10
}

pub struct Dog {
    pub fluffy: bool,
    pub age: i32,
}
impl Dog {
    pub fn woof(&self) -> i32 {
        fn duplicate_name() -> i32 {
            9999
        }
        self.age
            + self::duplicate_name()
            + self::super::super::utils::say_something::say_hello()
            + crate::utils::say_something::say_hello()
            + crate::colors::green::duplicate_name()
            + super::DOG_ACTIVITY
            + super::stuff_function()
    }
}
