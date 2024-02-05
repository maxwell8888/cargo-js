use crate::prelude::{web::Math, JsBoolean, JsNumber, JsNumberTrait};
use std::ops::Add;

use super::RustBool;

#[allow(dead_code)]
/// ```js
/// class RustInteger {
///     constructor(jsNumber) {
///         this.jsNumber = jsNumber;
///     }
///     add(other) {
///         return new RustInteger(this.jsNumber + other.jsNumber);
///     }
///     abs() {
///         return new RustInteger(Math.abs(this.jsNumber));
///     }
///     rem(other) {
///         return new RustInteger(this.jsNumber % other.jsNumber);
///     }
/// }
/// ```
struct RustInteger<T: JsNumberTrait> {
    js_number: JsNumber<T>,
}
impl<T: JsNumberTrait> RustInteger<T> {
    fn copy(&self) -> Self {
        RustInteger {
            js_number: self.js_number,
        }
    }
    fn eq(&self, other: &RustInteger<T>) -> RustBool {
        RustBool {
            js_boolean: JsBoolean(self.js_number == other.js_number),
        }
    }

    fn ne(&self, other: &RustInteger<T>) -> RustBool {
        RustBool {
            js_boolean: JsBoolean(self.js_number != other.js_number),
        }
    }
    // fn add(self, other: $t) -> $t { self + other }
    fn add(self, other: RustInteger<T>) -> Self {
        RustInteger {
            js_number: self.js_number + other.js_number,
        }
    }
    fn add_assign(&mut self, other: RustInteger<T>) {
        self.js_number.0 += other.js_number.0;
    }
    // pub const fn abs(self) -> Self {
    fn abs(self) -> Self {
        RustInteger {
            js_number: Math::abs(self.js_number),
        }
    }
    fn rem(self, other: RustInteger<T>) -> Self {
        RustInteger {
            js_number: self.js_number % other.js_number,
        }
    }
}

// Using JSON.stringify() with any BigInt value will raise a TypeError https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt#use_within_json
// Generally JSON doesn't handle BigInt well
// The Number.MAX_SAFE_INTEGER static data property represents the maximum safe integer in JavaScript (2^53 – 1)
// make usize be (2^53 – 1)?
// i32 (2^31 − 1)
// i64 (2^63 − 1)
// f32 ≈ 3.4028235 × 10^38 (≈ 2^127)
// f64 ≈ 1.7976931348623157 × 10^308 (≈ 2^1023)

struct RustFloat<T: JsNumberTrait> {
    js_number: JsNumber<T>,
}
impl<T: JsNumberTrait> RustFloat<T> {
    fn eq(&self, other: &RustFloat<T>) -> RustBool {
        RustBool {
            js_boolean: JsBoolean(self.js_number == other.js_number),
        }
    }

    fn ne(&self, other: &RustFloat<T>) -> RustBool {
        RustBool {
            js_boolean: JsBoolean(self.js_number != other.js_number),
        }
    }

    // fn add(self, other: $t) -> $t { self + other }
    fn add(self, other: RustFloat<T>) -> Self {
        RustFloat {
            js_number: self.js_number + other.js_number,
        }
    }
    // pub const fn abs(self) -> Self {
    fn abs(self) -> Self {
        RustFloat {
            js_number: Math::abs(self.js_number),
        }
    }

    /// Returns `e^(self)`, (the exponential function).
    ///
    /// # Examples
    ///
    /// ```
    /// let one = 1.0_f64;
    /// // e^1
    /// let e = one.exp();
    ///
    /// // ln(e) - 1 == 0
    /// let abs_difference = (e.ln() - 1.0).abs();
    ///
    /// assert!(abs_difference < 1e-10);
    /// ```
    pub fn exp(self) -> Self {
        RustFloat {
            js_number: Math::exp(self.js_number),
        }
    }
}
