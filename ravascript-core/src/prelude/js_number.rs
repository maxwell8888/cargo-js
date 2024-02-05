use std::ops::{Add, AddAssign, Rem};

// #[derive(Debug, Default)]
// struct JsNumber{}
// impl Add for JsNumber {

// }

// struct Math();
// impl Math {
//     fn abs(x: JsNumber) -> JsNumber {
//         JsNumber
//     }
// }

// i64 is too large for Number so need to use BigInt like `5` -> `5n`
// `JsNumber(5).js_number_method()` -> `5.jsNumberMethod()`
// `5.rust_number_method(&self)` -> `rustNumberMethod(5)`
// `5.rust_number_method(&mut self)` -> `Integer(5).rustNumberMethod()`
// For now, all 5 -> `Integer(5).rustNumberMethod()`

// NOTE JS can't call methods directly on number literals eg `5.toString()` and instead we have to paranthesise like `(5).toString()`
pub trait JsNumberTrait:
    Copy + Clone + PartialEq + Add<Self, Output = Self> + AddAssign + Rem<Self, Output = Self> + Sized
{
    fn abs(self) -> Self;
}
impl JsNumberTrait for i8 {
    fn abs(self) -> Self {
        self.abs()
    }
}
impl JsNumberTrait for i16 {
    fn abs(self) -> Self {
        self.abs()
    }
}
impl JsNumberTrait for i32 {
    fn abs(self) -> Self {
        self.abs()
    }
}
impl JsNumberTrait for i64 {
    fn abs(self) -> Self {
        self.abs()
    }
}
impl JsNumberTrait for f32 {
    fn abs(self) -> Self {
        self.abs()
    }
}
impl JsNumberTrait for f64 {
    fn abs(self) -> Self {
        self.abs()
    }
}
// If we want to transpile to a normal JS number, we must write it in rust like `JsNumber(5)`. This is because number literals like `5` will get directly transpiled to number wrappers like `RustInteger`
// TODO eventually we actually only want numbers that are used as mutable refs to transpile to `RustInteger`, and non mut ref numbers to transpile to normal js numbers.
// We have proper implementations here because even though they are not used when transpiling to JS, they are useful even we want to use these types in code compiled with rustc
#[derive(Copy, Clone, Debug)]
pub struct JsNumber<T: JsNumberTrait>(pub T);
impl<T: JsNumberTrait> JsNumber<T> {
    pub fn abs(self) -> Self {
        JsNumber(self.0.abs())
    }
}
impl<T: JsNumberTrait> PartialEq for JsNumber<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T: JsNumberTrait> Add for JsNumber<T> {
    type Output = JsNumber<T>;

    fn add(self, rhs: Self) -> Self::Output {
        JsNumber(self.0 + rhs.0)
    }
}
impl<T: JsNumberTrait> Rem for JsNumber<T> {
    type Output = JsNumber<T>;

    fn rem(self, rhs: Self) -> Self::Output {
        JsNumber(self.0 % rhs.0)
    }
}
