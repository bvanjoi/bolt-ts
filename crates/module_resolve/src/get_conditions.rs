use bolt_ts_config::{ModuleKind, NormalizedModuleResolution};

pub(crate) fn get_conditions<'a>(
    module_resolution: NormalizedModuleResolution,
    custom_conditions: impl Iterator<Item = &'a str>,
    no_dts_resolution: bool,
    resolution_mode: Option<ModuleKind>,
) -> Vec<String> {
    let resolution_mode = match resolution_mode {
        Some(mode) => Some(mode),
        None => match module_resolution {
            NormalizedModuleResolution::Bundler => Some(ModuleKind::ESNext),
            NormalizedModuleResolution::Node10 => return vec![],
            _ => None,
        },
    };

    let mut conditions = if resolution_mode == Some(ModuleKind::ESNext) {
        vec!["import".to_string()]
    } else {
        vec!["require".to_string()]
    };
    if !no_dts_resolution {
        conditions.push("types".to_string());
    };
    if !matches!(module_resolution, NormalizedModuleResolution::Bundler) {
        conditions.push("node".to_string());
    }
    for condition in custom_conditions {
        conditions.push(condition.to_string());
    }
    conditions
}

#[test]
fn test_get_conditions() {
    let conditions = get_conditions(
        NormalizedModuleResolution::Bundler,
        [].into_iter(),
        false,
        None,
    );
    assert_eq!(conditions, ["import", "types"]);

    let conditions = get_conditions(
        NormalizedModuleResolution::Node10,
        [].into_iter(),
        false,
        None,
    );
    assert!(conditions.is_empty());

    let conditions = get_conditions(
        NormalizedModuleResolution::Node10,
        [].into_iter(),
        true,
        None,
    );
    assert!(conditions.is_empty());

    let conditions = get_conditions(
        NormalizedModuleResolution::Node10,
        ["abc"].into_iter(),
        true,
        None,
    );
    assert!(conditions.is_empty());

    let conditions = get_conditions(
        NormalizedModuleResolution::Node10,
        [].into_iter(),
        false,
        Some(ModuleKind::ES2022),
    );
    assert_eq!(conditions, ["require", "types", "node"]);

    let conditions = get_conditions(
        NormalizedModuleResolution::Node10,
        ["abc"].into_iter(),
        false,
        Some(ModuleKind::ES2022),
    );
    assert_eq!(conditions, ["require", "types", "node", "abc"]);
}
