mod utils;
use pretty_assertions::{assert_eq, assert_ne};
use ravascript::prelude::web::{
    try_, Console, Document, Event, HTMLInputElement, JsError, Json, Node, SyntaxError, NAVIGATOR,
};
use ravascript::prelude::*;
use ravascript::{catch, try_};
use ravascript_core::{format_js, from_block, from_crate, generate_js_from_module};
use ravascript_macros::module_as_str;
use ravascript_macros::{fn_as_str, fn_stmts_as_str};
use tracing::{debug, info, trace};
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::{fmt, layer::SubscriberExt, EnvFilter, Registry};
use tracing_tree::HierarchicalLayer;
use utils::*;

pub fn setup_tracing() {
    if let Ok(level) = std::env::var("RUST_LOG") {
        // Configure a custom event formatter
        let format = fmt::format()
            .with_level(false) // don't include levels in formatted output
            .with_target(false) // don't include targets
            .with_thread_ids(false) // include the thread ID of the current thread
            .with_thread_names(false) // include the name of the current thread
            .pretty(); // use the `Compact` formatting style.

        let layer = HierarchicalLayer::default()
            .with_writer(std::io::stdout)
            .with_indent_lines(true)
            .with_indent_amount(2)
            // .with_thread_names(true)
            // .with_thread_ids(true)
            .with_verbose_exit(true)
            .with_verbose_entry(true);
        // .with_targets(true);
        Registry::default().with(layer).init();
        // Registry::default().with(HierarchicalLayer::new(2));

        // tracing::subscriber::set_global_default(subscriber).unwrap();

        // tracing_subscriber::fmt()
        //     .event_format(format)
        //     .with_env_filter(EnvFilter::new(format!("ravascript={level}")))
        //     .init();
    }
}

// #[ignore]
#[tokio::test]
async fn it_transpiles_crate_directory() {
    setup_tracing();

    let actual = from_crate("../for-testing".into(), false, false);

    let expected = r#"
    // crate
    function duplicateName() {
      return 10;
    }
    (function main() {
      duplicateName();
      utils__colors__duplicateName();
      var thing = External.new();
      var fido = new Green(true, 2);
      console.assert(fido.woof().eq(32));
    })();
    
    // foo_bar
    class Internal {
      constructor(age) {
        this.age = age;
      }
    
      addTen() {
        return this.age.add(10);
      }
    }
    class External {
      constructor(sub, count) {
        this.sub = sub;
        this.count = count;
      }
    
      static new() {
        return new External(new Internal(0), 9);
      }
    }
    
    // colors
    var DOG_ACTIVITY = 10;
    function colors__duplicateName() {
      return 10;
    }
    function stuffFunction() {
      return 10;
    }
    
    // colors::green
    function green__duplicateName() {
      return 3;
    }
    class Green {
      constructor(fluffy, age) {
        this.fluffy = fluffy;
        this.age = age;
      }
    
      woof() {
        function green__duplicateName() {
          return 9999;
        }
        return (
          this.age
          .add(green__duplicateName())
          .add(green__duplicateName())
          .add(sayHello())
          .add(duplicateName())
          .add(sayHello())
          .add(green__duplicateName())
          .add(DOG_ACTIVITY)
          .add(stuffFunction())
        );
      }
    }
    
    // utils
    
    // utils::say_something
    function sayHello() {
      return 10;
    }
    
    // utils::colors
    function utils__colors__duplicateName() {
      return 10;
    }
    "#;

    // println!("{}", expected);
    // println!("{}", format_js(expected));
    // println!("{}", &actual);
    // println!("{}", format_js(&actual));

    assert_eq!(format_js(expected), format_js(actual));
}

// TODO it might be better to rely on the for-testing dir for testing `crate` rather than using unchecked Rust
#[tokio::test]
async fn simple_module() {
    // let actual = r2j_file_unchecked!(
    // TODO I think I would actually prefer to have an explicit `mod wrapper { }` in cases like this. Whilst it is more verbose, it makes it much clearer what `self` if referring to.
    let actual = r2j_file!(
        struct Bar {}
        pub fn baz() {
            let _ = Bar {};
        }
        mod foo {
            fn green() {
                // let blue = crate::baz();
            }
        }
    );
    let expected = r#"// crate
class Bar {}
function baz() {
  var _ = new Bar();
}

// foo
function green() {}"#;
    assert_eq!(expected, actual);
}

#[tokio::test]
async fn module_super() {
    // let actual = r2j_file_unchecked!(
    // TODO I think I would actually prefer to have an explicit `mod wrapper { }` in cases like this. Whilst it is more verbose, it makes it much clearer what `self` if referring to.
    let actual = r2j_file!(
        struct Bar {}
        fn baz() {
            let _ = Bar {};
        }
        mod foo {
            fn green() {
                let blue = super::baz();
            }
        }
    );
    let expected = r#"// crate
class Bar {}
function baz() {
  var _ = new Bar();
}

// foo
function green() {
  var blue = baz();
}"#;
    assert_eq!(expected, actual);
}

#[tokio::test]
async fn module_self() {
    let actual = r2j_file!(
        struct Bar {}
        fn baz() {
            let _ = Bar {};
        }
        fn green() {
            let blue = self::baz();
        }
    );
    let expected = r#"class Bar {}
function baz() {
  var _ = new Bar();
}
function green() {
  var blue = baz();
}"#;
    assert_eq!(expected, actual);
}

#[tokio::test]
async fn module_crate() {
    // Can't be checked because `crate::` will be pointing at the wrong thing
    let actual = r2j_file_unchecked!(
        fn baz() -> i32 {
            5
        }
        mod foo {
            fn green() {
                let blue = crate::baz();
            }
        }
    );
    let expected = r#"// crate
function baz() {
  return 5;
}

// foo
function green() {
  var blue = baz();
}"#;
    assert_eq!(expected, actual);
}

// TODO why is this not checking anything?
#[tokio::test]
async fn use_paths() {
    let actual = r2j_file_run_main!(
        fn duplicate() -> i32 {
            0
        }
        mod one {
            pub fn duplicate() -> i32 {
                1
            }
            pub use two::three;
            pub mod two {
                fn duplicate() -> i32 {
                    2
                }
                pub mod three {
                    // Access private items in parent modules
                    pub fn duplicate() -> i32 {
                        super::super::duplicate() + super::duplicate()
                    }
                    pub mod another_one {
                        pub use super::super::super::duplicate;
                    }
                }
            }
        }
        mod four {
            use super::one;
            pub use one::two;
            pub fn duplicate() -> i32 {
                two::three::duplicate() + one::duplicate()
            }
        }
        mod five {
            use super::four::two;
            use two::three;
            pub fn duplicate() -> i32 {
                two::three::duplicate()
                    + super::one::three::another_one::duplicate()
                    + three::another_one::duplicate()
            }
        }
        fn main() {
            assert_eq!(duplicate(), 0);
            assert_eq!(one::duplicate(), 1);
            assert_eq!(one::two::three::duplicate(), 3);
            assert_eq!(four::duplicate(), 4);
            assert_eq!(five::duplicate(), 5);
        }
    );
    let actual = format_js(actual);

    let expected = r#"class Bar {}
function baz() {
  var _ = new Bar();
}
function green() {
  var blue = baz();
}"#;

    // let _ = execute_js_with_assertions(&actual).await.unwrap();
    // assert_eq!(expected, actual);
}
