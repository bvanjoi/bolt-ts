use std::path::Path;

pub fn is_case_sensitivity_absolute_path(mut p: &Path) -> bool {
    if !p.is_absolute() {
        return false;
    }
    loop {
        let parent = match p.parent() {
            Some(parent) => parent,
            None => return true,
        };
        let entires = match parent.read_dir() {
            Ok(entries) => entries,
            Err(_) => return true,
        };
        let has = entires
            .filter_map(|entry| entry.ok())
            .any(|entry| entry.path() == p);
        if !has {
            return false;
        }
        p = parent;
    }
}
