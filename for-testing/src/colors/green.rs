fn duplicate_name() -> i32 {
    3
}

pub struct Green {
    pub fluffy: bool,
    pub age: i32,
}
impl Green {
    pub fn woof(&self) {
        fn duplicate_name() -> i32 {
            9
        }
        assert!(self.age == 2);
        // assert!(self::duplicate_name() == 3); TODO Need to take scoped names into account when "deduplicating" module level names. Currently crate crate::duplicate_name get's parsed to duplicateName() which clashes with the scoped definiton in this fn
        assert!(duplicate_name() == 9);
        assert!(self::super::super::utils::say_something::say_hello() == 8);
        // assert!(crate::duplicate_name() == 10); TODO Need to take scoped names into account when "deduplicating" module level names. Currently crate crate::duplicate_name get's parsed to duplicateName() which clashes with the scoped definiton in this fn
        assert!(crate::utils::say_something::say_hello() == 8);
        // assert!(crate::colors::green::duplicate_name() == 3);
        assert!(crate::colors::duplicate_name() == 6);
        assert!(crate::utils::duplicate_name() == 7);
        assert!(super::DOG_ACTIVITY == 5);
        assert!(super::stuff_function() == 4);
    }
}
