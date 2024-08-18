use crate::make_item_definitions::{ItemActual, ItemRef, RustMod};

#[derive(Debug, Clone)]
pub struct Duplicate {
    pub namespace: Vec<String>,
    module_path: Vec<String>,
    pub name: String,
    pub original_module_path: Vec<String>,
}

pub fn namespace_duplicates(item_refs: &[ItemRef], item_defs: &[ItemActual]) -> Vec<Duplicate> {
    // TODO account for local functions which shadow these names
    // (name space, module path (which gets popped), name, original module path)

    // TODO confine use of camel and case_convert to handle_syn_*** so we can be confident we are dealing with snake_case

    // TODO try and use same code for counting/creating scopes for all passes to ensure they stay in sync

    // TODO tests

    // Extract vec: Vec<(module path, name)> of items from modules and scopes
    // NOTE I think it is impossible to do this without creating a new Vec because we need to be able to sort the Vec
    // NOTE would need to take into account scoped item names if we hoisted scoped items to module level
    // (module path, item name)

    fn recurse(
        rust_mods: &[&RustMod],
        item_defs: &[ItemActual],
        names_to_dedup: &mut Vec<(Vec<String>, String)>,
    ) {
        for rust_mod in rust_mods {
            for item in &rust_mod.items {
                if let ItemRef::Mod(rust_mod2) = item {
                    recurse(&[rust_mod2], item_defs, names_to_dedup);
                } else if let Some(index) = item.index() {
                    let actual = &item_defs[index];
                    names_to_dedup.push((rust_mod.module_path.clone(), actual.ident().to_owned()));
                }
            }
        }
    }

    let rust_mods = item_refs
        .iter()
        .filter_map(|item_ref| match item_ref {
            ItemRef::Mod(rust_mod) => Some(rust_mod),
            _ => None,
        })
        .collect::<Vec<_>>();

    // Simply gets module path and name of all module level (non scoped) items
    let mut names_to_dedup = Vec::new();
    // let scoped_names_to_dedup = Vec::new();
    recurse(&rust_mods, item_defs, &mut names_to_dedup);

    let mut duplicates = Vec::new();
    // push `Duplicate` for name that has is duplicated
    for name in &names_to_dedup {
        if names_to_dedup
            .iter()
            // .chain(scoped_names_to_dedup.iter())
            .filter(|(_module_path, name2)| &name.1 == name2)
            .count()
            > 1
        {
            let dup = Duplicate {
                namespace: Vec::new(),
                module_path: name.0.clone(),
                name: name.1.to_string(),
                original_module_path: name.0.clone(),
            };

            // To start off, add a single path segment to names that are duplicated by scoped items
            // let is_scoped_name = scoped_names_to_dedup.iter().any(|s| dup.name == s.1);
            // if is_scoped_name {
            //     dup.namespace.insert(0, dup.module_path.pop().unwrap())
            // }

            duplicates.push(dup);
        }
    }

    // If a duplicate is found add to it's "namespace" by popping from the (remaining) module path and inserting it at the beginning of the namespace
    loop {
        let found_duplicate = update_dup_names(&mut duplicates);
        if !found_duplicate {
            break;
        }
    }

    // Finally, add the item name to the namespace
    for dup in duplicates.iter_mut() {
        dup.namespace.push(dup.name.clone());
    }

    // TODO rather than storing a global list of duplicates, would it not make more sense to have a "dedup path" field on ItemDef etc? This might be more effeicient performance wise but would be harder to test? handle_item_struct etc don't actually lookup the ItemDef etc so it actually seems more straightforward to just keep the dups in a Vec, even though it would still be way more performany to lookup a known ItemDef on a known module.
    duplicates
}

fn update_dup_names(duplicates: &mut [Duplicate]) -> bool {
    let mut found_duplicate = false;

    // We take a copy to make sure both duplicates have a path segment added to their namespace, rather than just the first one that is found
    // NOTE we add to the name space of all duplicates rather than just one because they eventually diverge eg we end up with:
    // [green__foo__Bar] and [blue__foo__Bar]. (Though this case will be rare as it relies on having two submodules named `foo` in different parent modules `green` and `blue`, in most cases there won't be duplicate module names so we will end up with something like:
    // [green__Bar] and [blue__Bar])
    let dups_copy = duplicates.to_vec();
    for dup in duplicates.iter_mut() {
        // TODO why not use .any() ??
        if dups_copy
            .iter()
            .filter(|dup_copy| dup_copy.name == dup.name && dup_copy.namespace == dup.namespace)
            .count()
            > 1
        {
            // if dup.module_path != vec!["crate"] {
            //     dup.namespace.insert(0, dup.module_path.pop().unwrap())
            // }
            dup.namespace.insert(0, dup.module_path.pop().unwrap());
            found_duplicate = true;
        }
    }

    found_duplicate
}
