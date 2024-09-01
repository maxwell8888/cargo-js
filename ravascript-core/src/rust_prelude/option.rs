#![allow(dead_code, non_camel_case_types, unused_variables)]

// use super::RustBool;
// use crate::prelude::JsBoolean;

use std::marker::PhantomData;

pub enum Result<T, E> {
    Ok(T),
    Err(E),
}

// #[allow(dead_code)]
// #[derive(Copy, PartialOrd, Eq, Ord, Debug, Hash)]
// #[derive(PartialEq)]
// TODO don't want to force T to always be PartialEq?
// pub enum Option<T: PartialEq> {
pub enum Option<T> {
    Some(T),
    None,
}

// pub trait FnOnce<T> {}

// pub use self::Option::*;
// pub use Option::*;

// #[allow(dead_code)]
// impl<T: PartialEq> Option<T> {
impl<T> Option<T> {
    // fn eq(&self, other: &Option<T>) -> bool {
    //     // return new RustBool(this.id === other.id && this.data.eq(other.data));
    //     // This gets manually overriden
    //     match (self, other) {
    //         (Some(a), Some(b)) => a == b,
    //         (Some(_), None) => false,
    //         (None, Some(_)) => false,
    //         (None, None) => true,
    //     }
    // }

    // fn ne(&self, other: &Option<T>) -> bool {
    //     match (self, other) {
    //         (Some(a), Some(b)) => a != b,
    //         (Some(_), None) => true,
    //         (None, Some(_)) => true,
    //         (None, None) => false,
    //     }
    // }

    // pub fn is_some(&self) -> bool {
    //     matches!(*self, Some(_))
    // }

    pub fn is_some_and(self, f: impl FnOnce(T) -> bool) -> bool {
        match self {
            Option::Some(x) => f(x),
            Option::None => false,
        }
    }

    // pub fn is_none(&self) -> bool {
    //     !self.is_some()
    // }

    // TODO need to parse and handle panic! input properly
    // pub fn expect(self, msg: &str) -> T {
    //     match self {
    //         Some(val) => val,
    //         None => panic!("{}", msg),
    //     }
    // }

    pub fn unwrap(self) -> T {
        match self {
            Option::Some(val) => val,
            Option::None => panic!("called `Option::unwrap()` on a `None` value"),
        }
    }

    // pub fn unwrap_or(self, default: T) -> T {
    //     match self {
    //         Some(x) => x,
    //         None => default,
    //     }
    // }

    // pub fn unwrap_or_else<F>(self, f: F) -> T
    // where
    //     F: FnOnce() -> T,
    // {
    //     match self {
    //         Some(x) => x,
    //         None => f(),
    //     }
    // }

    // pub fn unwrap_or_default(self) -> T
    // where
    //     T: Default,
    // {
    //     match self {
    //         Some(x) => x,
    //         None => T::default(),
    //     }
    // }

    // pub fn map<U: PartialEq, F>(self, f: F) -> Option<U>
    // where
    //     F: FnOnce(T) -> U,
    // {
    //     match self {
    //         Some(x) => Some(f(x)),
    //         None => None,
    //     }
    // }

    // pub fn map_or<U, F>(self, default: U, f: F) -> U
    // where
    //     F: FnOnce(T) -> U,
    // {
    //     match self {
    //         Some(t) => f(t),
    //         None => default,
    //     }
    // }

    // pub fn map_or_else<U, D, F>(self, default: D, f: F) -> U
    // where
    //     D: FnOnce() -> U,
    //     F: FnOnce(T) -> U,
    // {
    //     match self {
    //         Some(t) => f(t),
    //         None => default(),
    //     }
    // }

    // pub fn ok_or<E>(self, err: E) -> Result<T, E> {
    //     match self {
    //         Some(v) => Ok(v),
    //         None => Err(err),
    //     }
    // }

    // pub fn ok_or_else<E, F>(self, err: F) -> Result<T, E>
    // where
    //     F: FnOnce() -> E,
    // {
    //     match self {
    //         Some(v) => Ok(v),
    //         None => Err(err()),
    //     }
    // }

    // pub fn and<U>(self, optb: Option<U>) -> Option<U> {
    //     match self {
    //         Some(_) => optb,
    //         None => None,
    //     }
    // }

    // pub fn and_then<U, F>(self, f: F) -> Option<U>
    // where
    //     F: FnOnce(T) -> Option<U>,
    // {
    //     match self {
    //         Some(x) => f(x),
    //         None => None,
    //     }
    // }

    // pub fn or(self, optb: Option<T>) -> Option<T> {
    //     match self {
    //         x @ Some(_) => x,
    //         None => optb,
    //     }
    // }

    // pub fn or_else<F>(self, f: F) -> Option<T>
    // where
    //     F: FnOnce() -> Option<T>,
    // {
    //     match self {
    //         x @ Some(_) => x,
    //         None => f(),
    //     }
    // }

    // pub fn xor(self, optb: Option<T>) -> Option<T> {
    //     match (self, optb) {
    //         (a @ Some(_), None) => a,
    //         (None, b @ Some(_)) => b,
    //         _ => None,
    //     }
    // }
}

// TODO even though i32 is Copy, we are only interested in *structs* which are Copy, though this might be wrong if is_copy is used for other purposes.
struct i32 {}

struct Vec<T: Copy> {
    deleteme: T,
}
impl<T: Copy> Vec<T> {
    fn iter(&self) -> Vec<T> {
        Vec {
            deleteme: self.deleteme,
        }
    }
    fn map<U>(&self, f: impl FnOnce(T) -> U) -> Vec<T> {
        Vec {
            deleteme: self.deleteme,
        }
    }
    fn collect(&self) -> Vec<T> {
        Vec {
            deleteme: self.deleteme,
        }
    }
}
struct String {}
impl String {
    fn to_string(&self) -> String {
        String {}
    }
    fn clone(&self) -> String {
        String {}
    }
    fn push_str(&self, string: String) -> String {
        String {}
    }
}

struct str {}
struct Bool {}
struct Box {}
