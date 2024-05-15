struct Internal {
    age: i32,
}
impl Internal {
    fn add_ten(&self) -> i32 {
        self.age + 10
    }
}

pub mod file_module_level_inline {
    pub fn duplicate_name() -> i32 {
        12
    }
    pub mod file_module_inline_sub {
        pub fn duplicate_name() -> i32 {
            13
        }
    }
}

pub struct External {
    sub: Internal,
    count: i32,
}
impl External {
    pub fn new() -> External {
        External {
            sub: Internal { age: 0 },
            count: 9,
        }
    }
}
