use bolt_ts_utils::path::{NormalizePath, path_as_str};
use std::path::{Path, PathBuf};

pub fn normalize_join(base: &Path, target: impl AsRef<Path>) -> PathBuf {
    debug_assert!(base.is_normalized());
    let t = if bolt_ts_path::get_root_length(path_as_str(&target)) != 0 {
        target.as_ref().normalize()
    } else {
        let mut result = base.to_path_buf();
        for comp in target.as_ref().components() {
            match comp {
                std::path::Component::ParentDir => {
                    if result.components().count() > 1 {
                        // Don't go beyond root
                        result.pop();
                    }
                }
                std::path::Component::CurDir => {}
                _ => {
                    result.push(comp);
                }
            }
        }
        if target.as_ref().need_trailing_slash() && result.as_os_str().as_encoded_bytes() != b"/" {
            result.as_mut_os_string().push("/");
        }
        result
    };

    debug_assert!(t.is_normalized(), "t: {t:#?}");
    t
}

#[test]
fn test_normalize_join() {
    let should_eq = |base_dir: &str, target: &str, expected: &str| {
        let base = Path::new(base_dir);
        let res = normalize_join(base, target);
        let res = path_as_str(&res);
        assert_eq!(res, expected);
    };

    should_eq("/a/b/c", "d", "/a/b/c/d");
    should_eq("/a/b/c", "..", "/a/b/");
    should_eq("/a/b/c", "./", "/a/b/c/");
    should_eq("/a/b/c", "./.", "/a/b/c/");
    should_eq("/a/b/c", ".", "/a/b/c/");
    should_eq("/a/b/c", "./..", "/a/b/");
    should_eq("/a/b/c", "./d", "/a/b/c/d");
    should_eq("/a/b/c", "./d/", "/a/b/c/d/");
    should_eq("/a/b/c", "../d", "/a/b/d");
    should_eq("/a/b/c", "../../d", "/a/d");
    should_eq("/a/b/c", "../../../d", "/d");
    should_eq("/a/b/c", "../../../../d", "/d");
    should_eq("/a/b/c", "../../../../d/", "/d/");

    should_eq("/a", "/d", "/d");
    should_eq("/a", "/d/e", "/d/e");
    should_eq("/a", "/d/e/", "/d/e/");
}
