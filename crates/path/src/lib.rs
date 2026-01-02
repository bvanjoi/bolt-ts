mod combine_paths;
mod get_root_length;
mod path_is_relative;

pub use self::combine_paths::combine_paths;
use self::get_root_length::get_encoded_root_length;
pub use self::get_root_length::get_root_length;
pub use self::path_is_relative::path_is_relative;
use bolt_ts_utils::path::{BACKSLASH, SLASH};

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

#[inline]
pub fn is_external_module_relative(module_name: &str) -> bool {
    path_is_relative(module_name) || is_rooted_disk_path(module_name)
}

pub fn is_rooted_disk_path(path: &str) -> bool {
    get_encoded_root_length(path) > 0
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
