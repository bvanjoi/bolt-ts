pub fn path_is_relative(path: &str) -> bool {
    path.starts_with("./")
        || path.starts_with("../")
        || path == "."
        || path == ".."
        || path.starts_with(".\\")
        || path.starts_with("..\\")
}

#[test]
fn test_path_is_relative() {
    assert!(path_is_relative("."));
    assert!(path_is_relative(".."));
    assert!(path_is_relative("./"));
    assert!(path_is_relative("../"));
    assert!(path_is_relative(".\\a"));
    assert!(path_is_relative("..\\a"));
    assert!(!path_is_relative("a"));
}
