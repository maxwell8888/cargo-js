#[allow(dead_code)]
// #[derive(Copy, PartialOrd, Eq, Ord, Debug, Hash)]
// #[derive(PartialEq)]
pub enum Option<T> {
    Some(T),
    None,
}
// pub use self::Option::*;
pub use Option::*;

#[allow(dead_code)]
impl<T> Option<T> {
    // pub fn is_some(&self) -> bool {
    //     matches!(*self, Some(_))
    // }

    pub fn is_some_and(self, f: impl FnOnce(T) -> bool) -> bool {
        match self {
            None => false,
            Some(x) => f(x),
        }
    }

    // pub fn is_none(&self) -> bool {
    //     !self.is_some()
    // }

    // TODO need to parse and handle panic! input properly
    pub fn expect(self, msg: &str) -> T {
        match self {
            Some(val) => val,
            None => panic!("{}", msg),
        }
    }

    pub fn unwrap(self) -> T {
        match self {
            Some(val) => val,
            None => panic!("called `Option::unwrap()` on a `None` value"),
        }
    }

    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Some(x) => x,
            None => default,
        }
    }

    pub fn unwrap_or_else<F>(self, f: F) -> T
    where
        F: FnOnce() -> T,
    {
        match self {
            Some(x) => x,
            None => f(),
        }
    }

    // pub fn unwrap_or_default(self) -> T
    // where
    //     T: Default,
    // {
    //     match self {
    //         Some(x) => x,
    //         None => T::default(),
    //     }
    // }

    pub fn map<U, F>(self, f: F) -> Option<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Some(x) => Some(f(x)),
            None => None,
        }
    }

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
