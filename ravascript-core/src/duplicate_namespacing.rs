use crate::make_item_definitions::ModuleData;

#[derive(Debug, Clone)]
pub struct Duplicate {
    pub namespace: Vec<String>,
    module_path: Vec<String>,
    pub name: String,
    pub original_module_path: Vec<String>,
}

pub fn namespace_duplicates(modules: &Vec<ModuleData>) -> Vec<Duplicate> {
    // TODO account for local functions which shadow these names
    // (name space, module path (which gets popped), name, original module path)

    // TODO confine use of camel and case_convert to handle_syn_*** so we can be confident we are dealing with snake_case

    // TODO try and use same code for counting/creating scopes for all passes to ensure they stay in sync

    // TODO tests

    // Extract vec: Vec<(module path, name)> of items from modules and scopes
    // NOTE I think it is impossible to do this without creating a new Vec because we need to be able to sort the Vec
    // NOTE would need to take into account scoped item names if we hoisted scoped items to module level
    // (module path, item name)
    let mut names_to_dedup = Vec::new();
    let mut scoped_names_to_dedup = Vec::new();
    for m in modules {
        for item_def in &m.various_definitions.item_definitons {
            names_to_dedup.push((&m.path, &item_def.ident));
        }
        for fn_info in &m.various_definitions.fn_info {
            names_to_dedup.push((&m.path, &fn_info.ident));
        }
        // TODO adding traits names means their names will be taken into account when finding duplicates and namespacing, which we don't always want because traits don't always actually appear in the transpiled JS
        for trait_def in &m.various_definitions.trait_definitons {
            names_to_dedup.push((&m.path, &trait_def.name));
        }
        for const_ in &m.various_definitions.consts {
            names_to_dedup.push((&m.path, &const_.name));
        }
        for svd in &m.scoped_various_definitions {
            for item_def in &svd.1.item_definitons {
                scoped_names_to_dedup.push((&m.path, &item_def.ident));
            }
            for fn_info in &svd.1.fn_info {
                scoped_names_to_dedup.push((&m.path, &fn_info.ident));
            }
            for trait_def in &svd.1.trait_definitons {
                scoped_names_to_dedup.push((&m.path, &trait_def.name));
            }
            for const_ in &svd.1.consts {
                scoped_names_to_dedup.push((&m.path, &const_.name));
            }
        }
    }
    let mut duplicates = Vec::new();
    for name in &names_to_dedup {
        if names_to_dedup
            .iter()
            // NOTE given we are also taking into account scoped names here, `duplicates` might have entries that are actually unique (ie only duplicated in different scopes? Doesn't make much sense, do we mean a single module level name that is only duplicated by scoped items?), but we still want them namespaced (why?), so this needs to be taken into account in `update_dup_names`
            .chain(scoped_names_to_dedup.iter())
            .filter(|(_module_path, name2)| &name.1 == name2)
            .count()
            > 1
        {
            let mut dup = Duplicate {
                namespace: Vec::new(),
                module_path: name.0.clone(),
                name: name.1.clone(),
                original_module_path: name.0.clone(),
            };

            // To start off, add a single path segment to names that are duplicated by scoped items
            let is_scoped_name = scoped_names_to_dedup.iter().any(|s| &dup.name == s.1);
            if is_scoped_name {
                dup.namespace.insert(0, dup.module_path.pop().unwrap())
            }

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
        if dups_copy
            .iter()
            .filter(|dup_copy| dup.name == dup_copy.name && dup.namespace == dup_copy.namespace)
            .count()
            > 1
        {
            if dup.module_path != vec!["crate"] {
                dup.namespace.insert(0, dup.module_path.pop().unwrap())
            }
            found_duplicate = true;
        }
    }

    found_duplicate
}
