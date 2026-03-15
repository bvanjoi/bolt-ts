use bolt_ts_config::NormalizedModuleResolution;

use super::ResolutionMode;
use super::ResolverOptions;

pub(crate) fn get_conditions<'a>(
    options: &ResolverOptions,
    resolution_mode: Option<ResolutionMode>,
) -> Vec<String> {
    let resolution_mode = match resolution_mode {
        Some(mode) => Some(mode),
        None => match options.module_resolution {
            NormalizedModuleResolution::Bundler => Some(ResolutionMode::ESNext),
            NormalizedModuleResolution::Node10 => return vec![],
            _ => None,
        },
    };

    let mut conditions = if resolution_mode == Some(ResolutionMode::ESNext) {
        vec!["import".to_string()]
    } else {
        vec!["require".to_string()]
    };
    if !options.flags.no_dts_resolution() {
        conditions.push("types".to_string());
    };
    if !matches!(
        options.module_resolution,
        NormalizedModuleResolution::Bundler
    ) {
        conditions.push("node".to_string());
    }
    for condition in options.custom_conditions {
        conditions.push(condition.to_string());
    }
    conditions
}

#[test]
fn test_get_conditions() {
    use super::ResolveFlags;
    let conditions = get_conditions(
        &ResolverOptions {
            module_resolution: NormalizedModuleResolution::Bundler,
            flags: ResolveFlags::empty(),
            custom_conditions: &[],
        },
        None,
    );
    assert_eq!(conditions, ["import", "types"]);

    let conditions = get_conditions(
        &ResolverOptions {
            module_resolution: NormalizedModuleResolution::Node10,
            flags: ResolveFlags::empty(),
            custom_conditions: &[],
        },
        None,
    );
    assert!(conditions.is_empty());

    let conditions = get_conditions(
        &ResolverOptions {
            module_resolution: NormalizedModuleResolution::Node10,
            flags: ResolveFlags::NO_DTS_RESOLUTION,
            custom_conditions: &[],
        },
        None,
    );
    assert!(conditions.is_empty());

    let conditions = get_conditions(
        &ResolverOptions {
            module_resolution: NormalizedModuleResolution::Node10,
            flags: ResolveFlags::NO_DTS_RESOLUTION,
            custom_conditions: &["abc".to_string()],
        },
        None,
    );
    assert!(conditions.is_empty());

    let conditions = get_conditions(
        &ResolverOptions {
            module_resolution: NormalizedModuleResolution::Node10,
            flags: ResolveFlags::empty(),
            custom_conditions: &[],
        },
        Some(ResolutionMode::ESNext),
    );
    assert_eq!(conditions, ["import", "types", "node"]);

    let conditions = get_conditions(
        &ResolverOptions {
            module_resolution: NormalizedModuleResolution::Node10,
            flags: ResolveFlags::empty(),
            custom_conditions: &["abc".to_string()],
        },
        Some(ResolutionMode::ESNext),
    );
    assert_eq!(conditions, ["import", "types", "node", "abc"]);
}
