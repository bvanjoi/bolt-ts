use std::fs::DirEntry;
use std::path::Path;

pub fn fixtures(project_root: &Path, sub: &str) -> Vec<DirEntry> {
    let test_dir = project_root.join(sub);
    let cases: Vec<DirEntry> = std::fs::read_dir(&test_dir)
        .unwrap_or_else(|_| {
            panic!("{} is not exist", test_dir.to_string_lossy());
        })
        .filter_map(|entry| entry.ok())
        .collect();
    cases
}
