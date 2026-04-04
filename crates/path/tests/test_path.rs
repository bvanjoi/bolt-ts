use bolt_ts_path::has_trailing_directory_separator;
use bolt_ts_path::remove_trailing_directory_separator;

#[test]
fn test_has_trailing_directory_separator() {
    assert!(has_trailing_directory_separator("/a/b/".as_bytes()));
    assert!(!has_trailing_directory_separator("/a/b".as_bytes()));
    assert!(has_trailing_directory_separator("/".as_bytes()));
}

#[test]
fn test_remove_trailing_directory_separator() {
    let mut path = std::path::PathBuf::from("/a/b/");
    remove_trailing_directory_separator(&mut path);
    assert_eq!(path, std::path::PathBuf::from("/a/b"));

    let mut path = std::path::PathBuf::from("/a/b");
    remove_trailing_directory_separator(&mut path);
    assert_eq!(path, std::path::PathBuf::from("/a/b"));
}
