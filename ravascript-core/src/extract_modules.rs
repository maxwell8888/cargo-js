use quote::quote;
use std::{fs, path::PathBuf};
use syn::{ImplItem, Item, ItemUse, Stmt, UseTree, Visibility};

use crate::{
    case_convert,
    js_ast::{DestructureObject, DestructureValue},
    GlobalDataScope,
};

// Similarly to `update_classes`, we need to do a pass to replace all use of top level items like `myFunc()`, `new SomeClass()`, `SomeClass.associatedFunc()` with `this.myFunc()`, `new this.SomeClass()`, `this.SomeClass.associatedFunc()`. This means first getting the names of the top level items, and then not just iterating through the module's statements but though every expression at a top level item could be called absolutely anywhere, eg in an `if` condition.
// What about where we are inside a class method so this refers to the class, not the module?
// Solutions:
// add a unique name like `this.moduleThis = this;` - not possible because it still exists on the `this` which class methods don't have access to.
// add `this.moduleThis` to class objects in init function??
// replace `myFunc()` with `this.thisModule.myFunc();` and add below to init script:
// this.colorModule.greenModule.greenClass.prototype.thisModule = this.colorModule.greenModule;`

// Having "crate" in the module path is useful for representing that the top level module is indeed a module, and for giving it a name that can be looked up in the list. However, it is annoying for eg using the path to create a filepath from

// TODO `names` is probably redundant as we have the same data in `modules`
// Update names
// Determine which names are not globally unique and need namespacing
// TODO should cache the syn::File parsed for each module to avoid repeating this expensive operation (read file and parse) during the parse stage
/// gets names of module level items, creates `ModuleData` for each module, and adds `use` data to module's `.use_mapping`
pub fn extract_modules(
    module_level_items: bool,
    items: &Vec<Item>,
    // Same as `global_data.crate_path`, used for prepending module filepaths, except we don't have a `GlobalData` yet so we pass it in directly
    // None if we are extracting data for a single file or snippet, rather than an actual crate (so no `mod foo` allowed)
    // TODO crate_path might use hiphens instead of underscore as a word seperator, so need to ensure it is only used for file paths, and not module paths
    crate_path: &Option<PathBuf>,
    current_path: &mut Vec<String>,
    modules: &mut Vec<ModuleDataFirstPass>,
) {
    // let mut module_path_with_crate = vec!["crate".to_string()];
    // module_path_with_crate.extend(module_path.clone());
    // let current_module_data = modules
    //     .iter_mut()
    //     .find(|module| module.path == *module_path)
    //     .unwrap();
    // let defined_names = &mut current_module_data.defined_names;

    // dbg!(&module_path);

    // TODO the code for eg module.item_definitions.push(...) is a duplicated also for scope.item_definitons.push(...). Remove this duplication.
    for item in items {
        match item {
            Item::Const(item_const) => {}
            Item::Enum(item_enum) => {}
            Item::ExternCrate(_) => todo!(),
            Item::Fn(item_fn) => {
                // Record scoped ident names so we can ensure any module level items with the same name are namespaced
                // TODO also need to be looking for names in blocks, if expressions, etc
                let items = item_fn
                    .clone()
                    .block
                    .stmts
                    .into_iter()
                    .filter_map(|s| match s {
                        Stmt::Item(item) => Some(item),
                        _ => None,
                    })
                    .collect::<Vec<_>>();

                extract_modules(false, &items, crate_path, current_path, modules)
            }
            Item::ForeignMod(_) => todo!(),
            Item::Impl(item_impl) => {
                // Record scoped ident names for deduping/namespacing
                let items = item_impl
                    .clone()
                    .items
                    .into_iter()
                    .filter_map(|impl_item| match impl_item {
                        ImplItem::Fn(impl_item_fn) => {
                            let items =
                                impl_item_fn.block.stmts.into_iter().filter_map(
                                    |stmt| match stmt {
                                        Stmt::Item(item) => Some(item),
                                        _ => None,
                                    },
                                );
                            Some(items)
                        }
                        _ => None,
                    })
                    .flatten()
                    .collect::<Vec<_>>();

                extract_modules(false, &items, crate_path, current_path, modules)
            }
            Item::Macro(_) => {}
            Item::Mod(item_mod) => {
                let module_data = modules.get_mut(current_path);
                match item_mod.vis {
                    Visibility::Public(_) => {
                        module_data.pub_submodules.push(item_mod.ident.to_string())
                    }
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => module_data
                        .private_submodules
                        .push(item_mod.ident.to_string()),
                }

                let parent_name = current_path.last().cloned();
                current_path.push(item_mod.ident.to_string());

                let mut partial_module_data = ModuleDataFirstPass::new(
                    item_mod.ident.to_string(),
                    // parent_name,
                    current_path.clone(),
                );

                // NOTE we do the `modules.push(ModuleData { ...` below because we need to get the module items from the different content/no content branches
                if let Some(content) = &item_mod.content {
                    partial_module_data.items.clone_from(&content.1);
                    modules.push(partial_module_data);

                    // TODO how does `mod bar { mod foo; }` work?
                    extract_modules(true, &content.1, crate_path, current_path, modules);
                } else if let Some(crate_path2) = crate_path {
                    let mut file_path = crate_path2.clone();
                    file_path.push("src");
                    // IMPORTANT TODO need to check for "crate" *and* "my_external_crate", and also use the corrent `crate_path`
                    if *current_path == ["crate"] {
                        file_path.push("main.rs");
                    } else {
                        let mut module_path_copy = current_path.clone();
                        // remove "crate"
                        module_path_copy.remove(0);
                        let last = module_path_copy.last_mut().unwrap();
                        last.push_str(".rs");
                        file_path.extend(module_path_copy);
                    }

                    let code = fs::read_to_string(&file_path).unwrap();
                    let file = syn::parse_file(&code).unwrap();

                    partial_module_data.items.clone_from(&file.items);
                    modules.push(partial_module_data);
                    extract_modules(true, &file.items, crate_path, current_path, modules);
                } else {
                    panic!("`mod foo` is not allowed in files/modules/snippets, only crates")
                }
                current_path.pop();
            }
            Item::Static(_) => todo!(),
            Item::Struct(item_struct) => {}
            Item::Trait(item_trait) => {}
            Item::TraitAlias(_) => todo!(),
            Item::Type(_) => todo!(),
            Item::Union(_) => todo!(),
            Item::Use(item_use) => {
                // TODO we are adding all use stmts the the module use mappings rather than accounting for when we are not at the top level so the stmts should be added to the scope? Also does `resolve_path()` account for the difference?
                let module = modules.get_mut(current_path);
                handle_item_use(item_use, ItemUseModuleOrScope::Module(module));
            }
            Item::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleDataFirstPass {
    pub name: String,
    /// NOTE the path includes the name of the module, eg the path to the crate module is ["crate"] not [].
    pub path: Vec<String>,
    // pub_definitions: Vec<String>,
    // private_definitions: Vec<String>,
    pub pub_submodules: Vec<String>,
    pub private_submodules: Vec<String>,
    /// (snake case item name, snake case use path)
    pub pub_use_mappings: Vec<(String, Vec<String>)>,
    pub private_use_mappings: Vec<(String, Vec<String>)>,

    // We need this for extract_data_populate_item_definitions which happens after the modules ModuleData has been created by extract_data, but is populating the `ItemDefiitions` etc, and needs access to the original items in the module for this
    pub items: Vec<Item>,
}
impl ModuleDataFirstPass {
    /// NOTE the path includes the name of the module, eg the path to the crate module is ["crate"] not [].
    pub fn new(name: String, path: Vec<String>) -> Self {
        ModuleDataFirstPass {
            name,
            path,
            pub_submodules: Vec::new(),
            private_submodules: Vec::new(),
            pub_use_mappings: Vec::new(),
            private_use_mappings: Vec::new(),
            items: Vec::new(),
        }
    }
}

trait GetModuleFirstPass {
    fn get_mut(&mut self, module_path: &[String]) -> &mut ModuleDataFirstPass;
}
impl GetModuleFirstPass for Vec<ModuleDataFirstPass> {
    fn get_mut(&mut self, module_path: &[String]) -> &mut ModuleDataFirstPass {
        self.iter_mut().find(|m| m.path == module_path).unwrap()
    }
}

pub enum ItemUseModuleOrScope<'a> {
    ExternalCrate,
    Module(&'a mut ModuleDataFirstPass),
    Scope(&'a mut GlobalDataScope),
}

/// Populates module pub/private and scoped `.use_mappings`s
pub fn handle_item_use(item_use: &ItemUse, item_use_module_or_scope: ItemUseModuleOrScope) {
    let public = matches!(item_use.vis, Visibility::Public(_));

    let (root_module_or_crate, sub_modules) = match &item_use.tree {
        UseTree::Path(use_path) => {
            // Capture the name of the root of the "use_mapping", ie not the root of the absolute path which would be a crate name but the root of this specific use path
            let root_module_or_crate = use_path.ident.to_string();
            // Recursively parse the `syn::UseTree` to JS AST `DestructureObject`
            let sub_modules = tree_to_destructure_object(&use_path.tree);
            (root_module_or_crate, sub_modules)
        }
        // TODO need to consider what a simple `use foo` means, since for modules this would be preceeded by `mod foo` which has the same effect?
        UseTree::Name(use_name) => todo!(),
        _ => panic!("root of use trees are always a path or name"),
    };

    // handle globs
    // if sub_modules.0.len() == 0 {
    //     // For now we are not handling globs but need to use them for using enum variants which we will then need to inject manually
    //     return;
    // }

    // if root_module_or_crate == "Sse" {
    //     JsStmt::Raw(SSE_RAW_FUNC.to_string())
    // }

    // } else if root_module_or_crate == "crate" {
    //     // If we import something from our crate, inline it (probably what we want for external crates too?)
    //     // A much simpler plan for now is to force defining the type in the JS file, and then export, rather than the other way round
    //     // Get the name of the item to be inlined
    //     todo!()

    let (_root_module_or_crate, item_paths) = match &item_use.tree {
        UseTree::Path(use_path) => {
            let root_module_or_crate = use_path.ident.to_string();

            let mut item_paths = Vec::new();
            let mut relative_path = vec![use_path.ident.to_string()];
            tree_parsing_for_boilerplate(&use_path.tree, &mut relative_path, &mut item_paths);
            (root_module_or_crate, item_paths)
        }
        // TODO need to consider what a simple `use foo` means, since for modules this would be preceeded by `mod foo` which has the same effect?
        UseTree::Name(_use_name) => todo!(),
        _ => panic!("root of use trees are always a path or name"),
    };

    // TODO we do want to do the JsLocal destructure thing if the use is not a top level item?
    // TODO this is probably also the correct place to determine if std stuff like HashMap needs flagging
    // if is_module {
    match item_use_module_or_scope {
        ItemUseModuleOrScope::ExternalCrate => {}
        ItemUseModuleOrScope::Module(module) => {
            for item_path in item_paths {
                // Get current module since it must already exist if we are in it
                match item_use.vis {
                    Visibility::Public(_) => module.pub_use_mappings.push(item_path),
                    Visibility::Restricted(_) => todo!(),
                    Visibility::Inherited => module.private_use_mappings.push(item_path),
                }
            }
        }
        ItemUseModuleOrScope::Scope(scope) => {
            for item_path in item_paths {
                // Get current module since it must already exist if we are in it
                match item_use.vis {
                    // TODO I believe the `pub` keyword for scoped `use` statements is irrelevant/redundant given that idents from scoped `use` statements aren't visible outside the scope. The only time the are relevant is if there is also a *scoped* module inside the same scope, but this seems pretty niche so we will not handle this case for now.
                    Visibility::Public(_) => todo!(),
                    Visibility::Restricted(_) => todo!(),
                    // TODO this assumes we are pushing to the var scope in the JS parsing pass, we don't currently do this and it wouldn't make sense to add it there because then the mappings wouldn't be available to be used during the populate item def passes, for looking up the types used in scoped item defs. We either need to:
                    // store the scope id in .use_mappings like we do for item defs
                    // completely remove use of .use_mappings and have .resovle_path resolve the use mappings itself
                    // implement the Rust AST/syn IR
                    Visibility::Inherited => scope.use_mappings.push(item_path),
                }
            }
        }
    }
}

// fn handle_item_use_tree(use_tree: &UseTree, sub_modules: &mut Vec<DestructureValue>) {
//     dbg!(use_tree);
//     match use_tree {
//         UseTree::Path(use_path) => {
//             let key = use_path.ident.to_string();
//             let value = handle_item_use_tree(&*use_path.tree, sub_modules, root_module_or_crate);
//             sub_modules.push(Des)
//         }
//         UseTree::Name(use_name) => sub_modules.push(use_name.ident.to_string()),
//         UseTree::Rename(_) => todo!(),
//         UseTree::Glob(_use_glob) => sub_modules.push("*".to_string()),
//         UseTree::Group(use_group) => use_group.items.iter().for_each(|item| match item {
//             UseTree::Path(_) => todo!(),
//             UseTree::Name(use_name) => sub_modules.push(use_name.ident.to_string()),
//             UseTree::Rename(_) => todo!(),
//             UseTree::Glob(_) => todo!(),
//             UseTree::Group(_) => todo!(),
//         }),
//     }
// }

fn tree_to_destructure_object(use_tree: &UseTree) -> DestructureObject {
    match use_tree {
        UseTree::Path(use_path) => DestructureObject(vec![DestructureValue::Nesting(
            // TODO shouldn't be converting case here
            case_convert(&use_path.ident),
            tree_to_destructure_object(&use_path.tree),
        )]),
        UseTree::Name(use_name) => DestructureObject(vec![DestructureValue::KeyName(
            case_convert(&use_name.ident),
        )]),
        UseTree::Rename(_) => todo!(),
        UseTree::Glob(_) => DestructureObject(vec![]),
        UseTree::Group(use_group) => DestructureObject(
            use_group
                .items
                .iter()
                .map(|item| match item {
                    UseTree::Path(use_path) => DestructureValue::Nesting(
                        case_convert(&use_path.ident),
                        tree_to_destructure_object(&use_path.tree),
                    ),
                    UseTree::Name(use_name) => {
                        DestructureValue::KeyName(case_convert(&use_name.ident))
                    }
                    UseTree::Rename(_) => todo!(),
                    UseTree::Glob(_) => todo!(),
                    UseTree::Group(_) => todo!(),
                })
                .collect::<Vec<_>>(),
        ),
    }
}

/// We want each of used items to return the name of the item, and the path relative to the root module, eg:
/// eg `use mod::sub_mod::{item1, item2, another_mod::item3}` -> [mod/sub_mod/item1, mod/sub_mod/item2, mod/sub_mod/another_mod/item3]
///
/// `relative_path` (snake) is a temporary var for building the relative path that gets copied into `items`
///
/// items is what gets stored in global_data  Vec<(item name (snake), relative path (snake))>
///
fn tree_parsing_for_boilerplate(
    use_tree: &UseTree,
    // We push to `relative_path` to build up a path for each of the items/modules "imported"/`use`d by the use stmt
    relative_path: &mut Vec<String>,
    items: &mut Vec<(String, Vec<String>)>,
) {
    match use_tree {
        UseTree::Path(use_path) => {
            relative_path.push(use_path.ident.to_string());
            tree_parsing_for_boilerplate(&use_path.tree, relative_path, items);
        }
        // NOTE a `syn::UseName` can the the ident for an item *or* a submodule that is being `use`d??
        UseTree::Name(use_name) => items.push((use_name.ident.to_string(), relative_path.clone())),
        UseTree::Rename(_) => todo!(),
        UseTree::Glob(_) => {
            println!("here");
            dbg!(relative_path);
            dbg!(items);
            println!("{}", quote! { #use_tree });
            todo!()
        }
        UseTree::Group(use_group) => {
            for item in &use_group.items {
                match item {
                    UseTree::Path(use_path) => {
                        // Create separate `relative_path`s for each "fork" created by the `UseGroup`
                        let mut new_relative_path = relative_path.clone();
                        new_relative_path.push(use_path.ident.to_string());
                        tree_parsing_for_boilerplate(&use_path.tree, &mut new_relative_path, items);
                    }
                    UseTree::Name(use_name) => {
                        items.push((use_name.ident.to_string(), relative_path.clone()));
                    }
                    UseTree::Rename(_) => todo!(),
                    UseTree::Glob(_) => todo!(),
                    UseTree::Group(_) => todo!(),
                }
            }
        }
    };
}
