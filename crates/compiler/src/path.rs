const SLASH: u8 = b'/';
const BACKSLASH: u8 = b'\\';

// pub(super) fn normalize_slashes(path: &str) -> String {
//     path.replace('\\', "/")
// }

// pub(super) fn remove_trailing_directory_separator(path: &str) -> String {
//     if path.len() > 1 && path.ends_with(DIRECTORY_SEPARATOR) {
//         path.trim_end_matches(DIRECTORY_SEPARATOR).to_string()
//     } else {
//         path.to_string()
//     }
// }

// fn equate_strings_case_sensitive(a: &str, b: &str) -> bool {
//     a == b
// }

// fn equate_strings_case_insensitive(a: &str, b: &str) -> bool {
//     a.eq_ignore_ascii_case(b)
// }

// pub(super) fn get_any_extension_from_path<'a>(
//     name: &'a str,
//     extensions: Option<&'a [&str]>,
//     ignore_case: bool,
// ) -> Option<&'a str> {
//     if let Some(exts) = extensions {
//         // 根据 ignore_case 选择比较函数，默认 false 为区分大小写
//         let compare = if ignore_case {
//             equate_strings_case_insensitive
//         } else {
//             equate_strings_case_sensitive
//         };
//         return get_any_extension_from_path_worker(
//             &remove_trailing_directory_separator(path),
//             exts,
//             compare,
//         );
//     }
//     let base_file_name = get_base_file_name(path);
//     if let Some(extension_index) = base_file_name.rfind('.') {
//         base_file_name[extension_index..].to_string()
//     } else {
//         String::new()
//     }
// }

// pub(super) fn get_base_file_name(
//     path: &str,
//     extensions: Option<&[&str]>,
//     ignore_case: Option<bool>,
// ) -> String {
//     let mut path = normalize_slashes(path);

//     let root_length = get_root_length(&path);
//     if root_length == path.len() {
//         return "".to_string();
//     }

//     path = remove_trailing_directory_separator(&path);

//     let current_root = get_root_length(&path);
//     let last_sep_index = path.rfind(DIRECTORY_SEPARATOR).unwrap_or(0);
//     let start = std::cmp::max(current_root, last_sep_index + 1);
//     let name = &path[start..];

//     let extension = if let (Some(ext_list), Some(ignore)) = (extensions, ignore_case) {
//         get_any_extension_from_path(name, ext_list, ignore)
//     } else {
//         None
//     };
//     match extension {
//         Some(ext) => name[..name.len() - ext.len()].to_string(),
//         None => name.to_string(),
//     }
// }

pub fn is_external_module_relative(module_name: &str) -> bool {
    path_is_relative(module_name) || is_rooted_disk_path(module_name)
}

pub fn path_is_relative(path: &str) -> bool {
    let re = regex::Regex::new(r"^\.\.?(?:$|[\\/])").unwrap();
    re.is_match(path)
}

pub fn is_rooted_disk_path(path: &str) -> bool {
    get_encoded_root_length(path) > 0
}

fn is_volume_character(ch: u8) -> bool {
    ch.is_ascii_uppercase() || ch.is_ascii_lowercase()
}

fn get_file_url_volume_separator_end(path: &str, start: usize) -> isize {
    let bytes = path.as_bytes();
    if bytes.get(start).is_some_and(|c| *c == b':') {
        return (start + 1) as isize;
    }
    if bytes.get(start).is_some_and(|c| *c == b'%')
        && bytes.get(start + 1).is_some_and(|c| *c == 0x33)
        && bytes
            .get(start + 2)
            .is_some_and(|c| *c == b'a' || *c == b'A')
    {
        return (start + 3) as isize;
    }
    -1
}

pub(crate) fn get_root_length(path: &str) -> usize {
    let root_length = get_encoded_root_length(path);
    let len = if root_length < 0 {
        -root_length + 1
    } else {
        root_length
    };
    assert!(len >= 0);
    len as usize
}

fn get_encoded_root_length(path: &str) -> isize {
    if path.is_empty() {
        return 0;
    }
    let bytes = path.as_bytes();
    let ch0 = bytes[0];

    // POSIX or UNC
    if ch0 == SLASH || ch0 == BACKSLASH {
        if bytes.get(1).is_none_or(|b| *b != ch0) {
            return 1;
        }
        let target_sep = if ch0 == SLASH { SLASH } else { BACKSLASH };
        if let Some(rel_index) = path[2..].find(target_sep as char) {
            let p1 = 2 + rel_index;
            return (p1 + 1) as isize;
        } else {
            return path.len() as isize;
        }
    }

    // DOS
    if is_volume_character(ch0) && bytes.get(1).is_some_and(|b| *b == b':') {
        if bytes
            .get(2)
            .copied()
            .is_some_and(|c| c == SLASH || c == BACKSLASH)
        {
            return 3;
        }
        if path.len() == 2 {
            return 2;
        }
    }

    // URL
    let url_scheme_separator = "://";
    let Some(scheme_end) = path.find(url_scheme_separator) else {
        // relative
        return 0;
    };
    let authority_start = scheme_end + url_scheme_separator.len();
    let Some(rel_authority_end) = path[authority_start..].find('/') else {
        return -(path.len() as isize + 1);
    };
    let authority_end = authority_start + rel_authority_end;
    let scheme = &path[..scheme_end];
    let authority = &path[authority_start..authority_end];
    if scheme == "file" && (authority.is_empty() || authority == "localhost") {
        if let Some(next_byte) = bytes.get(authority_end + 1).copied() {
            if is_volume_character(next_byte) {
                let volume_separator_end =
                    get_file_url_volume_separator_end(path, authority_end + 2);
                if volume_separator_end != -1 {
                    if let Some(b) = path.as_bytes().get(volume_separator_end as usize) {
                        if *b == SLASH {
                            return -((volume_separator_end + 1) + 1);
                        }
                    }
                    if volume_separator_end as usize == path.len() {
                        return -(volume_separator_end + 1);
                    }
                }
            }
        }
    }
    -((authority_end + 1) as isize + 1)
}

pub fn has_trailing_directory_separator(path: &[u8]) -> bool {
    path.last().is_some_and(|&b| b == SLASH || b == BACKSLASH)
}

pub fn ensure_trailing_directory_separator(mut path: Vec<u8>) -> Vec<u8> {
    if has_trailing_directory_separator(&path) {
        path
    } else {
        path.push(SLASH);
        path
    }
}

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
