# Rust to Javascript Transpiler

# Usage
```bash
cargo install --git https://github.com/maxwell8888/cargo-js cargo-js

"fn foo_bar(num: i32) -> &str { \"hello\" }" | cargo js

# write generated JS to the console
git clone git@github.com:maxwell8888/cargo-js.git
cargo js transpile --package for-testing/
```
# Goals

Better DX than typescript
Better optimisation than Google Closure Compiler

## Customisable compilation
Choose whether compilation should be optimized for:
*   Compile time - useful during development
*   Bundle size - useful for eg targetting users with a limited network
*   Runtime performance - useful for heavy apps doing lots of computation
Ideally should create a matrix of each variable, so developer can choose an optimal strategy that balances all three competing goals.
Ideally should be able to choose a strategy for individual each module/function, given one fn might be important for runtime performance, but for the rest of the app bundle size is more important.

# Todo
- [x] module level mod and use stmts
- [ ] mod and use within blocks
- [ ] mutable references to numbers and strings
- [ ] transpile lib crate to js file with exports of modules nested into objects
- [ ] splitting output across multiple js files
- [ ] macros

License
Licensed under either of

Apache License, Version 2.0 (LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0)
MIT license (LICENSE-MIT or http://opensource.org/licenses/MIT)
at your option.

Contribution
Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.