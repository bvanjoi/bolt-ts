use std::path::{Component, PathBuf};

use crate::{BACKSLASH, SLASH};

pub trait NormalizePath {
    fn normalize(&self) -> PathBuf;
    fn is_normalized(&self) -> bool;
    fn need_trailing_slash(&self) -> bool;
}

impl NormalizePath for std::path::Path {
    fn normalize(&self) -> PathBuf {
        let mut components = self.components().peekable();
        let mut ret = if let Some(c @ Component::Prefix(..)) = components.peek() {
            let buf = PathBuf::from(c.as_os_str());
            components.next();
            buf
        } else {
            PathBuf::new()
        };
        for component in components {
            match component {
                Component::Prefix(..) => unreachable!(),
                Component::RootDir => {
                    ret.push(component.as_os_str());
                }
                Component::CurDir => {}
                Component::ParentDir => {
                    ret.pop();
                }
                Component::Normal(c) => {
                    ret.push(c);
                }
            }
        }

        if self.need_trailing_slash() {
            ret.as_mut_os_string().push("/");
        }
        ret
    }

    fn is_normalized(&self) -> bool {
        self.as_os_str().as_encoded_bytes() == self.normalize().as_os_str().as_encoded_bytes()
    }

    fn need_trailing_slash(&self) -> bool {
        let bytes = self.as_os_str().as_encoded_bytes();
        bytes != b"/"
            && bytes
                .last()
                .is_some_and(|c| matches!(*c, SLASH | BACKSLASH | b'.'))
    }
}

#[test]
fn test_normalize_path() {
    let should_eq = |input: &str, expected: &str| {
        let actual = PathBuf::from(input).normalize();
        let actual = super::path_as_str(&actual);
        assert_eq!(actual, expected);
    };
    should_eq("/", "/");
    should_eq("/a/b/c", "/a/b/c");
    should_eq("/a/b/c/", "/a/b/c/");
    should_eq("/a/b/c/./", "/a/b/c/");
    should_eq("/a/b/c/.", "/a/b/c/");
    should_eq("/a/b/c/..", "/a/b/");
    should_eq("/a/b/c/./d", "/a/b/c/d");
    should_eq("/a/b/c/../d", "/a/b/d");
    should_eq("/a/b/c/./d/", "/a/b/c/d/");
    should_eq("/a/b/c/../d/", "/a/b/d/");
}

#[test]
fn test_is_normalized() {
    let normalized = |input: &str| {
        let actual = PathBuf::from(input).is_normalized();
        assert!(actual);
    };
    let not_normalized = |input: &str| {
        let actual = PathBuf::from(input).is_normalized();
        assert!(!actual);
    };

    normalized("/a/b/c");
    normalized("/a/b/c/");
    normalized("/");
    not_normalized("/a/b/c/./");
    not_normalized("/a/b/c/.");
    not_normalized("/a/b/c/..");
}
