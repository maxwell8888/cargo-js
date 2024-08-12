#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(unused_imports)]

pub use ravascript_core::*;
pub use ravascript_macros::include_ravascript;
// pub mod prelude;

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf};

    use prettify_js::prettyprint;
    use ravascript_macros::{fn_as_str, module_as_str};
    // use crate::testing;

    use super::*;

    #[test]
    fn it_transpiles_local_variables() {
        #[fn_as_str]
        fn the_rust() {
            let myvar = 5;
        }
        let rust_str = the_rust_code_str();
        // let generated_js = from_fn(rust_str)
        //     .iter()
        //     .map(|stmt| stmt.js_string())
        //     .collect::<Vec<_>>()
        //     .join("\n");
        // assert_eq!(generated_js, "let myvar = 5;");
    }

    // #[test]
    // fn it_transpiles_structs_and_impl_methods() {
    //     #[module_as_str]
    //     mod the_rust {
    //         struct MyStruct {
    //             my_field: i32,
    //         }
    //     }
    //     let rust_str = the_rust_code_str();
    //     let mods = from_module(rust_str);
    //     dbg!(mods.len());
    //     let generated_js = mods
    //         .iter()
    //         .map(|stmt| stmt.js_string())
    //         .collect::<Vec<_>>()
    //         .join("\n");
    //     let (generated_js, _) = prettyprint(generated_js.as_str());
    //     let target_js = include_str!("../testing/structs_and_impl_methods.js");
    //     let (target_js, _) = prettyprint(target_js);

    //     assert_eq!(generated_js, target_js);
    // }

    fn get_rust_module_and_expected_js(dir_path: PathBuf) {
        let dir_name = dir_path.file_name().unwrap();

        let rust_file_name = format!("{}.rs", dir_name.to_string_lossy());
        let rust_file_path = dir_path.join(rust_file_name);
        dbg!(&rust_file_path);
        let rust_input = fs::read_to_string(rust_file_path).unwrap();

        let js_file_name = format!("{}.js", dir_name.to_string_lossy());
        let js_file_path = dir_path.join(js_file_name);
        let expected_js = fs::read_to_string(js_file_path).unwrap();

        let stmts = from_file(&rust_input, false);
        let generated_js = stmts
            .iter()
            .map(|stmt| stmt.js_string())
            .collect::<Vec<_>>()
            .join("\n");
        let (generated_js, _) = prettyprint(generated_js.as_str());
        let (target_js, _) = prettyprint(&expected_js);

        assert_eq!(generated_js, target_js);
    }

    // #[test]
    // fn it_transpiles_struct_no_new() {
    //     get_rust_module_and_expected_js("testing/struct_no_new".into());
    // }

    // #[test]
    // fn it_transpiles_structs_and_impl_methods() {
    //     get_rust_module_and_expected_js("testing/structs_and_impl_methods".into());
    // }

    // #[test]
    // fn it_transpiles_comprehsive_example() {
    //     let stmts = stmts_with_main(from_crate("../comprehensive-example/src/main.rs".into()));

    //     let generated_js = stmts
    //         .iter()
    //         .map(|stmt| stmt.js_string())
    //         .collect::<Vec<_>>()
    //         .join("\n");
    //     let (generated_js, _) = prettyprint(generated_js.as_str());
    //     fs::write("../ignore/comprehensive-example.js", &generated_js).unwrap();
    //     let target_js = include_str!("../testing/structs_and_impl_methods.js");
    //     let (target_js, _) = prettyprint(target_js);

    //     assert_eq!(generated_js, target_js);
    // }

    // #[test]
    // fn it_transpiles_comprehsive_example() {
    //     let stmts = stmts_with_main(from_crate("../comprehensive-example/src/main.rs".into()));

    //     let generated_js = stmts
    //         .iter()
    //         .map(|stmt| stmt.js_string())
    //         .collect::<Vec<_>>()
    //         .join("\n");
    //     let (generated_js, _) = prettyprint(generated_js.as_str());
    //     fs::write("../ignore/comprehensive-example.js", &generated_js).unwrap();
    //     let target_js = include_str!("../testing/structs_and_impl_methods.js");
    //     let (target_js, _) = prettyprint(target_js);

    //     assert_eq!(generated_js, target_js);
    // }
}
