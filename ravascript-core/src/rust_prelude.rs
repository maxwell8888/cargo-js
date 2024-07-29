// These files are only used for generating the JavaScript Rust types prelude. Because JS primitives are represented as "newtype"s, it is not always possible to write implementations that can be directly transpiled to JS, eg because of need to access the inner value like `self.js_number.0`, so in some cases cargo-js will manually replace implementations of methods with certain names like `add_assign`. In either case, the Rust implementation matches the behaviour of the JS implementation which is necessary because the types are used for....

#![allow(unused_imports)]

mod option;
pub use option::*;

mod number;
pub use number::*;

mod string;
pub use string::*;

mod bool;
pub use bool::*;
