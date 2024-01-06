fn duplicate_name() -> i32 {
    3
}

pub struct Green {
    pub fluffy: bool,
    pub age: i32,
}
impl Green {
    pub fn woof(&self) -> i32 {
        fn duplicate_name() -> i32 {
            9999
        }
        self.age
            + self::duplicate_name()
            // TODO shadowed names not impl yet
            + duplicate_name()  
            + self::super::super::utils::say_something::say_hello()
            // + crate::duplicate_name()
            // + crate::utils::say_something::say_hello()
            // + crate::colors::green::duplicate_name()
            // + super::DOG_ACTIVITY
            // + super::stuff_function()
    }
}
