// Using single integration test binary to reduce incremental compile times from over 2mins to around 10secs. See https://matklad.github.io/2021/02/27/delete-cargo-integration-tests.html and https://doc.rust-lang.org/cargo/guide/project-layout.html

#![allow(clippy::disallowed_names)]

mod destructuring;
mod enum_tests;
mod fn_and_closures;
mod if_let;
mod impl_tests;
mod mod_and_use;
mod mutate_copyable_structs;
mod mutate_integers;
mod mutate_strings;
mod numbers;
mod option;
mod other;
mod result;
mod struct_tests;
mod test_the_preludes;
mod trait_tests;
mod web;

mod utils;
