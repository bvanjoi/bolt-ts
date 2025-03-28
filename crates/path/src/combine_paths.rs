use crate::{ensure_trailing_directory_separator, get_root_length};

pub fn combine_paths(path: &str, paths: &[&str]) -> String {
    let mut valid_path_index = None;
    for (i, relative_path) in paths.iter().enumerate() {
        if get_root_length(relative_path) != 0 {
            valid_path_index = Some(i);
        }
    }
    let p = if let Some(valid_path_index) = valid_path_index {
        paths[valid_path_index]
    } else {
        path
    };
    let mut res = String::with_capacity(p.len() * 2);
    res.push_str(p);
    let mut res = res.into_bytes();
    let start = valid_path_index.map(|i| i + 1).unwrap_or(0);
    for relative_path in paths.iter().skip(start) {
        res = ensure_trailing_directory_separator(res);
        res.extend_from_slice(relative_path.as_bytes());
    }
    unsafe { String::from_utf8_unchecked(res) }
}

#[test]
fn test_combine_paths() {
    assert_eq!(
        combine_paths("path", &["to", "file.ext"]),
        "path/to/file.ext"
    );
    assert_eq!(
        combine_paths("/path", &["dir", "..", "to", "file.ext"]),
        "/path/dir/../to/file.ext"
    );
    assert_eq!(combine_paths("/path", &["/to", "file.ext"]), "/to/file.ext");
}
