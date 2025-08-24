#[cfg(test)]
fn parse_package_name(name: &str) -> (&str, Option<&str>) {
    let slash_idx = split_slash_for_package_name(name);
    if let Some(slash_idx) = slash_idx {
        let (namespace, package_name) = name.split_at(slash_idx);
        let package_name = &package_name[1..];
        (namespace, Some(package_name))
    } else {
        (name, None)
    }
}

pub fn split_slash_for_package_name(name: &str) -> Option<usize> {
    let has_namespace_scope = name.starts_with('@');
    let slash_index_list: Vec<usize> = name
        .chars()
        .enumerate()
        .filter_map(|(index, char)| if '/' == char { Some(index) } else { None })
        .collect();
    if has_namespace_scope {
        slash_index_list.get(1)
    } else {
        slash_index_list.first()
    }
    .copied()
}

#[test]
fn test_parse_package_name() {
    assert_eq!(parse_package_name("foo"), ("foo", None));
    assert_eq!(parse_package_name("@foo/bar"), ("@foo/bar", None));
    assert_eq!(
        parse_package_name("@foo/bar/baz"),
        ("@foo/bar", Some("baz"))
    );
    assert_eq!(parse_package_name("foo/bar/baz"), ("foo", Some("bar/baz")));
}
