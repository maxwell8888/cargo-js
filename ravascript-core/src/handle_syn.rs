mod handle_syn_expr;
mod handle_syn_item;
mod handle_syn_stmt;

pub use handle_syn_item::*;
pub use handle_syn_stmt::handle_stmt;
use tracing::debug;

use crate::{GlobalData, RustPathSegment, RustType, RustTypeImplTrait, RustTypeParam, RustTypeParamValue};
use quote::quote;
use syn::{GenericArgument, PathArguments, Type, TypeParamBound};

// TODO ideally test tracing output in get_path test cases to ensure the expect code path is being taken??
// TODO need to make sure this looks up traits as well as other items
/// -> (current module (during recursion)/item module path (upon final return), found item path, found item scope id)
///
/// TODO maybe should return Option<Vec<String>> for the module path to make it consistent with the rest of the codebase, but just returning a bool is cleaner
///
/// TODO given eg `use MyEnum::{Variant1, Variant2};` we need to not only look for match `ItemDefintion`s but also matching enum variants
#[allow(clippy::too_many_arguments)]
pub fn resolve_path(
    look_for_scoped_vars: bool,
    // TODO can we combine this with `look_for_scoped_vars`?
    look_for_scoped_items: bool,
    use_private_items: bool,
    mut segs: Vec<RustPathSegment>,
    // TODO replace GlobalData with `.modules` and `.scopes` to making setting up test cases easier
    global_data: &GlobalData,
    current_mod: &[String],
    // Only used to determine if current module is the original module
    orig_mod: &[String],
    current_scope_id: &Option<Vec<usize>>,
) -> (Vec<String>, Vec<RustPathSegment>, Option<Vec<usize>>) {
    debug!(segs = ?segs, "get_path_without_namespacing");

    // TODO I don't think we need to pass in the module `ModuleData` if we are already passing the `current_module` module path we can just use that to look it up each time, which might be less efficient since we shouldn't need to lookup the module if we haven't changed modules (though I think we are pretty much always changing modules except for use statements?), but we definitely don't want to pass in both. Maybe only pass in `module: &ModuleData` and not `current_module`
    // assert!(current_module == &module.path);

    let module = global_data
        .modules
        .iter()
        .find(|m| m.path == current_mod)
        .unwrap();

    // dbg!(&segs);
    // dbg!(&current_mod);
    // dbg!(&orig_mod);
    // dbg!(&current_scope_id);
    // dbg!(&module.scoped_various_definitions);

    let is_parent_or_same_module = if orig_mod.len() >= current_mod.len() {
        current_mod
            .iter()
            .enumerate()
            .all(|(i, current_module)| current_module == &orig_mod[i])
    } else {
        false
    };

    let item_defined_in_module = module.item_defined_in_module(
        use_private_items || is_parent_or_same_module,
        &segs[0].ident,
    );

    let path_starts_with_sub_module = module.path_starts_with_sub_module(
        use_private_items || is_parent_or_same_module,
        &segs[0].ident,
    );

    // TODO only look through transparent scopes
    let scoped_use_mapping = global_data
        .scopes
        .iter()
        .rev()
        .find_map(|s| s.use_mappings.iter().find(|u| u.0 == segs[0].ident));
    let mut use_mappings = module.pub_use_mappings.iter();
    let matched_use_mapping = if scoped_use_mapping.is_some() {
        scoped_use_mapping
    } else if use_private_items || is_parent_or_same_module {
        use_mappings
            .chain(module.private_use_mappings.iter())
            .find(|use_mapping| use_mapping.0 == segs[0].ident)
    } else {
        use_mappings.find(|use_mapping| use_mapping.0 == segs[0].ident)
    };
    // dbg!(matched_use_mapping);

    // TODO can module shadow external crate names? In which case we need to look for modules first? I think we do this implicitly based on the order of the if statements below?
    // TODO actually look up external crates in Cargo.toml
    let external_crate_names = ["web_prelude"];
    let path_is_external_crate = external_crate_names.iter().any(|cn| cn == &segs[0].ident);

    // TODO IMPORTANT we have two `is_scoped` vars here because we are using `get_path` in different contexts. `is_scoped_static` is for getting the path from static data, before syn -> JS parsing, and `is_scoped` is for use during the syn -> JS parsing. This needs thinking about, reconciling and simplifying. Should just stop using get_path for vars.
    let is_scoped_static_scope = if let Some(scope_id) = current_scope_id {
        // dbg!(scope_id);
        let mut temp_scope_id = scope_id.clone();
        // TODO initially wanted to not handle vars in this fn (`get_path`) but it doesn't seem possible without duplicating work given that an items and vars can shadow each other, we need to look through both the static and var scopes simultaneously to check if we have a var
        let mut is_var = false;
        let mut is_func = false;
        let mut is_item_def = false;
        let mut is_trait_def = false;
        while !temp_scope_id.is_empty() {
            let svd = module
                .scoped_various_definitions
                .iter()
                // TODO should this be `&svd.0 == temp_scope_id` ???
                .find(|svd| svd.0 == temp_scope_id);

            let svd = if let Some(svd) = svd {
                svd
            } else {
                println!("oh no, in get_path we couldn't find a scoped VariousDefinitions for one of the parent scopes of:");
                dbg!(scope_id);
                dbg!(&segs);
                dbg!(&module.scoped_various_definitions);
                panic!("");
            };
            // dbg!(&look_for_scoped_items);

            let static_scope = &svd.1;

            // NOTE the `segs.len() == 1` is important to differentiate between the receiver var `self` and the path to the current module `self::Foo` since the former will always be len == 1 and the latter len > 2
            is_var = look_for_scoped_vars && segs.len() == 1 && {
                // dbg!(&global_data.scopes);
                // dbg!(&scope_id);
                // dbg!(&segs[0].ident);
                // dbg!(&look_for_scoped_vars);

                let var_scope = global_data
                    .scopes
                    .iter()
                    .find(|s| s.scope_id == temp_scope_id)
                    .unwrap();
                var_scope
                    .variables
                    .iter()
                    .any(|var| var.name == segs[0].ident)
            };
            // dbg!(is_var);
            is_func = look_for_scoped_items
                && static_scope
                    .fn_info
                    .iter()
                    .any(|func| func.ident == segs[0].ident);

            // dbg!(&static_scope.item_definitons);
            // dbg!(&module.scoped_various_definitions);
            // dbg!(&segs[0].ident);
            // dbg!(&look_for_scoped_items);
            is_item_def = look_for_scoped_items
                && static_scope
                    .item_definitons
                    .iter()
                    .any(|item_def| item_def.ident == segs[0].ident);
            is_trait_def = look_for_scoped_items
                && static_scope
                    .trait_definitons
                    .iter()
                    .any(|trait_def| trait_def.name == segs[0].ident);

            if is_var || is_func || is_item_def || is_trait_def {
                break;
            }
            temp_scope_id.pop();
        }

        // A scoped item must be the first element in the segs, ie in the original module so we need `current_module == original_module`
        // TODO I don't think `&& current_mod == orig_mod` is necessary given look_for_scoped_vars and look_for_scoped_items
        if (is_var || is_func || is_item_def || is_trait_def) && current_mod == orig_mod {
            Some(temp_scope_id)
        } else {
            None
        }
    } else {
        None
    };

    // let is_scoped = global_data.scopes.iter().rev().any(|scope| {
    //     let is_func =
    //         look_for_scoped_items && scope.fns.iter().any(|func| func.ident == segs[0].ident);
    //     let is_item_def = look_for_scoped_items
    //         && scope
    //             .item_definitons
    //             .iter()
    //             .any(|item_def| item_def.ident == segs[0].ident);
    //     // TODO IMPORTANT
    //     // We cannot have a scoped item/fn definition with the same ident as a module, but we can have a scoped *var* with the same ident as a module/item/fn. I think Rust just chooses which one to use based on the context eg foo::bar must be an item/module, foo.bar() must be an instance/var, etc. To follow this approach would mean we need more context for this fn.
    //     // eg this is aloud:
    //     // use tracing;
    //     // let tracing = "ohno";
    //     // As far as I am aware the only common context in which a path might have length=1 is a use statement, which doesn't use this fn to resolve the path (though it probably should given we have to follow the same crate/self/super logic?) so for now just assume that if we match a scoped var name and the length is 1, then return the scoped var, even though technically it could be a module/item/fn eg this is valid: *NO* what about a simple `Foo {}` which is a path with length 1 where we could also have `let Foo = 5;`, which would make it impossible to decide which to return, eg module level struct (Some("crate"), ["Foo"]) vs scoped var (None, ["Foo"]).**
    //     // struct foo {}
    //     // fn main() {
    //     //     let foo = 5;
    //     // }
    //     // For scoped modules/items/fns there is currently no difference anyway since we currently just return Vec<RustPathSegment> regardless.
    //     // The main problem is the above example. However, the below is not valid which I believe demonstrates that it the ident must be unambigious if it can be used as eg a fn argument, and so it is indeed the context of where the path is being used eg `bar(foo);`, `let bar = foo;`, `foo {}`, etc which determines which thing to use. I think it will be non trivial to pass handle handle the different contexts to this fn.
    //     // struct foo;
    //     // fn main() {
    //     //     let foo = 5;
    //     // }
    //     // ** The distinction is between where the site of the path expects a definition (eg fn input type) and instances (eg assign to a variable). A slight complication is that both of the below are valid, just not simultaneously, but it still means that a path passed the the rhs of an assignment could be and instance *or* a definition, so we need to look for other if one doesn't exist, **but *only* if we have `struct foo;` and not `struct foo {}` because for the latter we *are* allowed both idents in scope, so need to ensure we choose the scoped var, in this case. This is as apposed to say the type of a fn input where we can always be sure to not look for scoped vars, only any items/fns.
    //     // let foo = 5;
    //     // let which = foo;
    //     // ...
    //     // struct foo;
    //     // let which = foo;
    //     // So I think maybe the trick is to pass an argument to this fn to say wether we should be considering vars and follow these rules:
    //     // 1. If including vars (eg simple 1-len path assignment) then look for a var first, else look for a struct, this way we will catch assigning `struct foo;` because a `foo` var can't exist in this case, and for `struct Foo {}` we will correctly pick the var first.
    //     // 2. If not inlcluding vars we simply don't have to look for vars.
    //     // Don't need both can just always do step 1?

    //     // self could be and instance or module path ie `fn foo(&self) { self }` or `self::MyStruct`. I can't think of any situations where a module path can
    //     let is_var = scope.variables.iter().any(|var| var.name == segs[0].ident)
    //         && segs.len() == 1
    //         && look_for_scoped_vars;

    //     // A scoped item must be the first element in the segs, ie in the original module so we need `current_module == original_module`
    //     // TODO I don't think `&& current_mod == orig_mod` is necessary given look_for_scoped_vars and look_for_scoped_items
    //     (is_func || is_item_def || is_var) && current_mod == orig_mod
    // });

    // dbg!(&global_data.scopes);

    // TODO not sure why we need use_private_items here
    // if use_private_items && is_scoped {
    // if is_scoped || is_scoped_static {
    // dbg!(&is_scoped_static_scope);
    if is_scoped_static_scope.is_some() {
        // Variables and scoped items
        // Need to handle scoped vars and items first, otherwise when handling as module paths, we would always first have to check if the path is a scoped var/item

        // If we are returning a scoped var/item, no recursion should have occured so we should be in the same module
        assert!(current_mod == orig_mod);
        // (current_mod.clone(), segs, is_scoped_static)
        (current_mod.to_vec(), segs, is_scoped_static_scope)
    } else if item_defined_in_module {
        (current_mod.to_vec(), segs, None)
    } else if segs[0].ident == "super" {
        // TODO if a module level item name is shadowed by an item in a fn scope, then module level item needs to be namespaced
        segs.remove(0);

        let mut current_module = current_mod.to_vec();
        current_module.pop();

        resolve_path(
            false,
            false,
            true,
            segs,
            global_data,
            &current_module,
            orig_mod,
            &None,
        )
    } else if segs[0].ident == "self" {
        // NOTE private items are still accessible from the module via self
        segs.remove(0);

        resolve_path(
            false,
            false,
            true,
            segs,
            global_data,
            current_mod,
            orig_mod,
            &None,
        )
    } else if segs[0].ident == "crate" {
        let current_module = vec!["crate".to_string()];

        segs.remove(0);

        resolve_path(
            false,
            false,
            true,
            segs,
            global_data,
            &current_module,
            orig_mod,
            &None,
        )
    } else if path_starts_with_sub_module {
        // Path starts with a submodule of the current module
        let mut submod_path = current_mod.to_vec();
        submod_path.push(segs[0].ident.to_string());

        segs.remove(0);

        resolve_path(
            false,
            false,
            false,
            segs,
            global_data,
            &submod_path,
            orig_mod,
            &None,
        )
    } else if let Some(use_mapping) = matched_use_mapping {
        // Use mappings the resolved path for each item/module "imported" into the module with a use statement. eg a module containing
        // `use super::super::some_module::another_module;` will have a use mapping recorded of eg ("another_module", ["crate", "top_module", "some_module"])
        // So say we have a path like `another_module::MyStruct;`, then we will match this use mapping and the below code combines the path from the mapping and the current "segs" to make `"crate", "top_module", "some_module", "another_module", "MyStruct";`
        // What if we have a path like `another_module::yet_another_module::MyStruct;`??

        // TODO I think we need to set the current_module to use_mapping.1, remove this from segs (ie just not add it), and then we can just start from that module in the next get_path iteration?? NO That causes tests to fail

        let mut use_segs = use_mapping.1.clone();
        use_segs.push(use_mapping.0.clone());
        // TODO IMPORTANT seems like we are not correctly populating turbofish here
        let mut use_segs = use_segs
            .into_iter()
            .map(|s| RustPathSegment {
                ident: s,
                turbofish: Vec::new(),
            })
            .collect::<Vec<_>>();
        segs.remove(0);
        use_segs.extend(segs);

        // TODO IMPORTANT for a `use` statement for a third party crate, we need to set the `current_module` accordingly. I think it is fine to just use ["name_of_crate", "module_in_crate", etc].

        // TODO do we not need to update the current module if the use path/mapping has taken us to a new module?? write some tests NO because the use mapping just provides an absolute path to the module/item that is used which is of course valid from the current module. The important part is that the get_path iteration after this `matched_use_mapping` iteration

        // let new_mod = if use_segs[0].ident == "crate" {
        //     // current_mod
        //     use_mapping.1.clone()
        // } else if external_crate_names
        //     .iter()
        //     .any(|cn| cn == &use_segs[0].ident)
        // {
        //     use_mapping.1.clone()
        // } else {
        //     panic!()
        // };

        resolve_path(
            false,
            false,
            true,
            use_segs,
            global_data,
            // &new_mod,
            current_mod,
            // &use_mapping.1.clone(),
            orig_mod,
            &None,
        )
    // } else if segs.len() == 1 && segs[0] == "this" {
    //     segs
    } else if path_is_external_crate {
        // } else if false {
        // TODO need to update current_mod

        // Handle equivalently to segs[0] == "crate"
        let crate_name = segs.remove(0);
        let current_module = [crate_name.ident].to_vec();

        resolve_path(
            false,
            false,
            true,
            segs,
            global_data,
            &current_module,
            orig_mod,
            &None,
        )
    } else {
        // Handle third party crates
        // TODO lookup available crates in Cargo.toml
        // if segs[0..3].iter().map(|s| &s.ident).collect::<Vec<_>>()
        //     == ["ravascript", "prelude", "web"]
        // {
        //     return (
        //         vec!["prelude_special_case".to_string()],
        //         segs[3..].to_vec(),
        //         false,
        //     );
        // }

        // If we can't find the ident anywhere, the only remaining possibility is that we have a prelude type
        assert_eq!(current_mod, orig_mod);
        assert!(segs.len() == 1 || segs.len() == 2);
        let seg = &segs[0];
        if seg.ident == "i32"
            || seg.ident == "String"
            || seg.ident == "str"
            || seg.ident == "bool"
            || seg.ident == "Some"
            || seg.ident == "None"
            || (seg.ident == "Box" && &segs[1].ident == "new")
            || seg.ident == "FnOnce"
            || seg.ident == "Copy"
            || seg.ident == "Vec"
        {
            // TODO IMPORTANT we aren't meant to be handling these in get_path, they should be handled in the item def passes, not the JS parsing. add a panic!() here. NO not true, we will have i32, String, etc in closure defs, type def for var assignments, etc.
            // TODO properly encode "prelude_special_case" in a type rather than a String
            (vec!["prelude_special_case".to_string()], segs, None)
        } else {
            dbg!("resolve_path couldn't find path");
            // dbg!(module);
            dbg!(current_mod);
            dbg!(current_scope_id);
            dbg!(segs);
            panic!()
        }
    }
}

/// Only used when handling syn closures and locals
/// We can't use this for fns/methods *return types* because we need to actually parse the body to differentiate between self and another var with the type Self.
/// So in all we are (*should be) using this for parsing: field types (which are kind of like self anyway?), and fn/method inputs
/// * fn has "return_type" in the name...
///
/// Remember this is only for definitions, but the types used *within* definitions can have resolved generics
///
/// NOTE also using this to parse local stmt type annotations eg `let foo: i32 = 5;`
///
/// TODO also using this for return types in handle_item_fn and not sure if that is reasonable
/// TODO handle types with len > 1
fn parse_fn_input_or_field(
    type_: &Type,
    has_mut_keyword: bool,
    // TODO if this is for passing types used in a parent item definition, surely the parent items generics cannot have been made concrete, in which case this should be &Vec<String> instead of &Vec<RustTypeParam>?
    parent_item_definition_generics: &[RustTypeParam],
    // TODO should just store the current module in GlobalData to save having to pass this around everywhere
    current_module: &[String],
    global_data: &mut GlobalData,
) -> RustType {
    debug!(type_ = ?quote! { #type_ }.to_string(), "parse_fn_input_or_field");
    match type_ {
        Type::Array(_) => todo!(),
        Type::BareFn(_) => todo!(),
        Type::Group(_) => todo!(),
        Type::ImplTrait(type_impl_trait) => {
            // TODO handle len > 1
            let _type_param_bound = type_impl_trait.bounds.first().unwrap();
            // get_return_type_of_type_param_bound(
            //     type_param_bound,
            //     parent_item_definition_generics,
            //     current_module,
            //     global_data,
            // )
            let bounds = type_impl_trait
                .bounds
                .iter()
                .filter_map(|b| {
                    match b {
                        TypeParamBound::Trait(trait_bound) => {
                            // TODO need to resolve trait names/paths with the module they are defined in and store that so we can look them up properly
                            if trait_bound.path.segments.len() > 1 {
                                todo!();
                            }
                            // TODO handle segment arguments because we might have eg `impl GenericFooTrait<Bar>`
                            let trait_name = trait_bound
                                .path
                                .segments
                                .iter()
                                .map(|t| t.ident.to_string())
                                .collect::<Vec<_>>();
                            // let poo = trait_name.into_iter().map(|fart| fart);
                            // TODO lookup trait in global data to get module path
                            let (module_path, found_scope_id, trait_definition) = global_data
                                .lookup_trait_definition_any_module(
                                    current_module,
                                    &global_data.scope_id_as_option(),
                                    trait_name,
                                );
                            Some((
                                module_path,
                                found_scope_id,
                                RustTypeImplTrait::SimpleTrait(trait_definition.name.clone()),
                            ))
                        }
                        TypeParamBound::Lifetime(_) => None,
                        TypeParamBound::Verbatim(_) => todo!(),
                        _ => todo!(),
                    }
                })
                .collect::<Vec<_>>();

            RustType::ImplTrait(bounds)
        }
        Type::Infer(_) => todo!(),
        Type::Macro(_) => todo!(),
        Type::Never(_) => todo!(),
        Type::Paren(_) => todo!(),
        Type::Path(type_path) => {
            // eg:
            // Foo<i32>
            // Foo<T>
            // Self (which given we are dealing with field or input, *must* be a different instance/type from self)
            // T (where the types bounds of T are elsewhere eg `where T: FooTrait` or `<T: FooTrait>`)
            // T: impl FooTrait

            // If it is a field, a type with a generic like T or Foo<T> means the generic depends on the parent so for
            // let foo = Foo { gen_field: i32 };
            // let field = foo.gen_field;
            // If we have just stored that the type of gen_field is T, we can just resolve T to i32 because we will haev the type of foo which will contain the resolved generics.
            // What happens if it hasn't been resolved yet? eg it might get resolved by being passed as an argument to a fn, so the arg type defines it? Should be fine as long as know we have an unresolved generic, so can (should be able to) look up the input types for the fn/method
            if type_path.path.segments.len() == 1 {
                let seg = type_path.path.segments.first().unwrap();
                let seg_name = seg.ident.to_string();
                let seg_name_str = seg_name.as_str();

                // Look to see if name is a generic which has been declared
                let generic = parent_item_definition_generics
                    .iter()
                    .rev()
                    .find(|generic| generic.name == seg_name_str);
                if let Some(generic) = generic {
                    // return match &generic.type_ {
                    //     RustTypeParamValue::Unresolved => {
                    //         dbg!(generic);
                    //         todo!()
                    //     }
                    //     RustTypeParamValue::RustType(rust_type) => *rust_type.clone(),
                    // };
                    return RustType::TypeParam(generic.clone());
                }

                // For fns:
                // the names of generics should be stored on FnInfo
                // Sometimes we can work out what the type of a generic is, eg it impls Fn in which case we only care about the return type, but mostly the generic will just be eg T, maybe with some trait bound, but that doesn't help us determine what the actual type is. We need to record where the generic type is inferred from, eg:
                // 1. the generic is used as the type for an input: easy, just check the type of the thing that eventually gets passed as an arg
                // 2. the generic is used as the return type: redundant, we don't need to know the type since we already determine the return type from the body

                // For structs
                // Can always be inferred from the arguments used to construct the struct?

                // For impl blocks
                match seg_name_str {
                    // "i32" => RustType::I32,
                    "bool" => RustType::Bool,
                    // "str" => RustType::String,
                    "Option" => {
                        let generic_type = match &seg.arguments {
                            PathArguments::AngleBracketed(angle_bracketed_generic_arguments) => {
                                match angle_bracketed_generic_arguments.args.first().unwrap() {
                                    GenericArgument::Lifetime(_) => todo!(),
                                    GenericArgument::Type(type_) => parse_fn_input_or_field(
                                        type_,
                                        has_mut_keyword,
                                        parent_item_definition_generics,
                                        current_module,
                                        global_data,
                                    ),
                                    GenericArgument::Const(_) => todo!(),
                                    GenericArgument::AssocType(_) => todo!(),
                                    GenericArgument::AssocConst(_) => todo!(),
                                    GenericArgument::Constraint(_) => todo!(),
                                    _ => todo!(),
                                }
                            }
                            _ => todo!(),
                        };
                        RustType::Option(RustTypeParam {
                            // TODO "T" shouldn't be hardcoded here
                            name: "T".to_string(),
                            type_: RustTypeParamValue::RustType(Box::new(generic_type)),
                        })
                    }
                    // "RustInteger" => {RustType::Struct(StructOrEnum { ident: "RustInteger".to_string(), members: (), generics: (), syn_object: () }),
                    // "RustFloat" => RustType::Struct(StructOrEnum { ident: "RustFloat".to_string(), members: (), generics: (), syn_object: () }),
                    // "RustString" => RustType::Struct(StructOrEnum { ident: "RustString".to_string(), members: (), generics: (), syn_object: () }),
                    // "RustBool" => RustType::Struct(StructOrEnum { ident: "RustBool".to_string(), members: (), generics: (), syn_object: () }),
                    struct_or_enum_name => {
                        // dbg!(current_module);
                        // dbg!(&global_data.scope_id_as_option());
                        // dbg!(&vec![struct_or_enum_name.to_string()]);
                        let (item_definition_module_path, resolved_scope_id, item_definition) =
                            global_data.lookup_item_definition_any_module_or_scope(
                                current_module,
                                &global_data.scope_id_as_option(),
                                &[struct_or_enum_name.to_string()],
                            );

                        // Look to see if any of the item's type params have been specified (they *must* have been specified, because you can't use a type without specifiying prodviding it's type params so they must either be concrete types, or use one of the parents params)
                        let item_type_params = match &seg.arguments {
                            PathArguments::None => Vec::new(),
                            PathArguments::AngleBracketed(gen_args) => {
                                gen_args
                                    .args
                                    .iter()
                                    .enumerate()
                                    .filter_map(|(i, a)| match a {
                                        GenericArgument::Lifetime(_) => None,
                                        GenericArgument::Type(type_) => {
                                            // Get the name of the generic from the items definition
                                            let gen_arg_name = item_definition.generics[i].clone();

                                            // First check if type is one of the parent's generics - why? purely so that we know whether the type is a generic or concrete type, we could equally look up the concrete type and assume that if that fails it must be a generic
                                            let type_param = if let Some(parent_generic) =
                                                parent_item_definition_generics
                                                    .iter()
                                                    .find(|g| g.name == gen_arg_name)
                                            {
                                                match parent_generic.type_ {
                                                    RustTypeParamValue::Unresolved => {
                                                        RustTypeParam {
                                                            name: gen_arg_name,
                                                            type_: RustTypeParamValue::Unresolved,
                                                        }
                                                    }
                                                    // "an item definition should not have any resovled type params"
                                                    RustTypeParamValue::RustType(_) => panic!(),
                                                }
                                            } else {
                                                // Otherwise we have another type we need to parse
                                                let param_rust_type = parse_fn_input_or_field(
                                                    type_,
                                                    has_mut_keyword,
                                                    parent_item_definition_generics,
                                                    current_module,
                                                    global_data,
                                                );
                                                RustTypeParam {
                                                    name: gen_arg_name,
                                                    type_: RustTypeParamValue::RustType(Box::new(
                                                        param_rust_type,
                                                    )),
                                                }
                                            };
                                            Some(type_param)
                                        }
                                        GenericArgument::Const(_) => todo!(),
                                        GenericArgument::AssocType(_) => todo!(),
                                        GenericArgument::AssocConst(_) => todo!(),
                                        GenericArgument::Constraint(_) => todo!(),
                                        _ => todo!(),
                                    })
                                    .collect::<Vec<_>>()
                            }
                            PathArguments::Parenthesized(_) => todo!(),
                        };

                        // match item_definition.syn_object {
                        //     StructOrEnumSynObject::Struct(_) => RustType::StructOrEnum(item_type_params, item_module_path, item_definition.ident.to_string()),
                        //     StructOrEnumSynObject::Enum(_) => RustType::Enum(item_type_params, item_module_path, item_definition.ident.to_string()),
                        // }

                        if item_definition_module_path == vec!["prelude_special_case".to_string()] {
                            if item_definition.ident == "i32" {
                                if has_mut_keyword {
                                    global_data.rust_prelude_types.rust_integer = true;
                                }
                                RustType::I32
                            } else if item_definition.ident == "String"
                                || item_definition.ident == "str"
                            {
                                if has_mut_keyword {
                                    global_data.rust_prelude_types.rust_string = true;
                                }
                                RustType::String
                            } else {
                                todo!()
                            }
                        } else {
                            RustType::StructOrEnum(
                                item_type_params,
                                item_definition_module_path,
                                resolved_scope_id,
                                item_definition.ident.to_string(),
                            )
                        }
                    }
                }
            } else {
                todo!()
            }
        }
        Type::Ptr(_) => todo!(),
        Type::Reference(type_reference) => {
            // let type_ = parse_type(&type_reference.elem);
            // let type_ = match type_ {
            //     TypeOrVar::RustType(rust_type) => rust_type,
            //     TypeOrVar::Unknown => RustType::Unknown,
            // };
            // TypeOrVar::Var(ScopedVar {
            //     name: "donotuse".to_string(),
            //     mut_: false,
            //     mut_ref: type_reference.mutability.is_some(),
            //     type_,
            // })
            let type_ = parse_fn_input_or_field(
                &type_reference.elem,
                has_mut_keyword,
                parent_item_definition_generics,
                current_module,
                global_data,
            );
            if type_reference.mutability.is_some() {
                match type_ {
                    RustType::I32 => {
                        global_data.rust_prelude_types.rust_integer = true;
                    }
                    RustType::F32 => todo!(),
                    RustType::Bool => todo!(),
                    RustType::String => {
                        global_data.rust_prelude_types.rust_string = true;
                    }
                    _ => {}
                }
                RustType::MutRef(Box::new(type_))
            } else {
                // RustType::Ref(Box::new(type_))
                type_
            }
        }
        Type::Slice(_) => todo!(),
        Type::TraitObject(_) => todo!(),
        Type::Tuple(_) => todo!(),
        Type::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}
