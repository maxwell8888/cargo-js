// In the long it might be better to use the actual rust std lib? But having our own means we can annotate the definitons so we don't have to handle everything in code which would be much more verbose and less clear than attributes/annotations on source code. And as long as we include this source in the binary then users can still use the normal rust std lib. Although while it would be annoying/a paper cut/extra complexity for users to need to add our alternative std lib, it would be useful in that they could jump to our source to see how we handle/annotate different stuff. We can just have it as optional? Add the alternate crate if you want to be able do this otherwise can just use the Rust std lib as normal.

// pub mod panic {
//     use javascript::{catch, try_};
//     // use std::any::Any;

//     pub enum MyError {}

//     // TODO use generic error instead of MyError
//     // fn catch_unwind<F: FnOnce() -> R + UnwindSafe, R>(f: F) -> Result<R> {}
//     // pub fn catch_unwind<F: FnOnce() -> R, R>(f: F) -> Result<R, Box<dyn Any + Send + 'static>> {
//     pub fn catch_unwind<F: FnOnce() -> R, R>(f: F) -> Result<R, MyError> {
//         try_! {{
//             return Ok(f());
//         }}
//         catch! {err, MyError,{
//             return Err(err);
//         }}
//     }
// }

pub mod marker {
    pub struct PhantomData;
}
