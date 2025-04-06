use crate::{BACKSLASH, SLASH};

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

pub fn get_root_length(path: &str) -> usize {
    let root_length = get_encoded_root_length(path);
    let len = if root_length < 0 {
        -root_length + 1
    } else {
        root_length
    };
    assert!(len >= 0);
    len as usize
}

pub(crate) fn get_encoded_root_length(path: &str) -> isize {
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

#[test]
fn test_get_root_length() {
    assert_eq!(get_root_length("a"), 0);
    assert_eq!(get_root_length("/"), 1);
    assert_eq!(get_root_length("c:"), 2);
    assert_eq!(get_root_length("c:d"), 0);
    assert_eq!(get_root_length("c:/"), 3);
    assert_eq!(get_root_length("c:\\"), 3);
}
