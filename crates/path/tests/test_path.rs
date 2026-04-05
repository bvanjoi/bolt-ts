use bolt_ts_path::get_base_path;
use bolt_ts_path::has_trailing_directory_separator;
use bolt_ts_path::is_rooted_disk_path;
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

#[test]
fn test_is_rooted_disk_path() {
    assert!(is_rooted_disk_path("c:/"));
    assert!(is_rooted_disk_path("c:\\"));
    assert!(is_rooted_disk_path("c:"));
    assert!(is_rooted_disk_path("/abc/def"));
    assert!(!is_rooted_disk_path("./relative"));
}

#[test]
fn test_get_base_path() {
    fn assert(path: &str, include: &str, expected: &str) {
        let path = std::path::Path::new(path);
        let base_path = get_base_path(path, include);
        assert_eq!(base_path.as_ref(), std::path::Path::new(expected));
    }
    assert("/a/b", "./c/", "/a/b/c");
    assert("/a/b", "./c", "/a/b/c");
    assert("/a/b", "./c/*.ts", "/a/b/c");
    assert("/a/b", "./c/*/d", "/a/b/c");
    assert("/a/b", "**/*", "/a/b");
    assert("/a/b/", "**/*", "/a/b/");
}
