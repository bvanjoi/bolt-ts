use bolt_ts_config::NormalizedModuleResolution;

use super::ResolutionMode;
use super::ResolverOptions;

#[derive(Clone, Copy, Debug)]
pub(crate) struct Conditions<'options> {
    by_resolution: &'static [&'static str],
    custom: &'options [String],
}

impl<'options> Conditions<'options> {
    pub fn is_empty(&self) -> bool {
        self.by_resolution.is_empty() && self.custom.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &'options str> {
        self.by_resolution
            .iter()
            .copied()
            .chain(self.custom.iter().map(|s| s.as_str()))
    }

    pub fn contains(&self, condition: &str) -> bool {
        self.by_resolution.contains(&condition) || self.custom.iter().any(|c| c == condition)
    }
}

impl<'a> Iterator for Conditions<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((first, rest)) = self.by_resolution.split_first() {
            self.by_resolution = rest;
            Some(*first)
        } else if let Some((first, rest)) = self.custom.split_first() {
            self.custom = rest;
            Some(first.as_str()) // 显式转换，更清晰
        } else {
            None
        }
    }
}

pub(crate) fn get_conditions<'options>(
    options: &'options ResolverOptions,
    resolution_mode: Option<ResolutionMode>,
) -> Conditions<'options> {
    let resolution_mode = match resolution_mode {
        Some(mode) => Some(mode),
        None => match options.module_resolution {
            NormalizedModuleResolution::Bundler => Some(ResolutionMode::ESNext),
            NormalizedModuleResolution::Node10 => {
                return Conditions {
                    by_resolution: &[],
                    custom: &[],
                };
            }
            _ => None,
        },
    };

    let use_import = resolution_mode == Some(ResolutionMode::ESNext);
    let no_dts_resolution = options.flags.no_dts_resolution();
    let is_bundler_resolution = matches!(
        options.module_resolution,
        NormalizedModuleResolution::Bundler
    );
    let conditions: &'static [&'static str] =
        match (use_import, no_dts_resolution, is_bundler_resolution) {
            (true, true, true) => &["import"],
            (true, true, false) => &["import", "node"],
            (true, false, true) => &["import", "types"],
            (true, false, false) => &["import", "types", "node"],
            (false, true, true) => &["require"],
            (false, true, false) => &["require", "node"],
            (false, false, true) => &["require", "types"],
            (false, false, false) => &["require", "types", "node"],
        };

    Conditions {
        by_resolution: conditions,
        custom: options.custom_conditions,
    }
}

#[test]
fn test_get_conditions() {
    use super::ResolveFlags;
    let options = ResolverOptions {
        module_resolution: NormalizedModuleResolution::Bundler,
        flags: ResolveFlags::empty(),
        custom_conditions: &[],
    };
    let conditions = get_conditions(&options, None);
    assert_eq!(conditions.by_resolution, ["import", "types"]);

    let options = ResolverOptions {
        module_resolution: NormalizedModuleResolution::Node10,
        flags: ResolveFlags::empty(),
        custom_conditions: &[],
    };
    let conditions = get_conditions(&options, None);
    assert!(conditions.is_empty());

    let options = ResolverOptions {
        module_resolution: NormalizedModuleResolution::Node10,
        flags: ResolveFlags::NO_DTS_RESOLUTION,
        custom_conditions: &[],
    };
    let conditions = get_conditions(&options, None);
    assert!(conditions.is_empty());

    let options = ResolverOptions {
        module_resolution: NormalizedModuleResolution::Node10,
        flags: ResolveFlags::NO_DTS_RESOLUTION,
        custom_conditions: &["abc".to_string()],
    };
    let conditions = get_conditions(&options, None);
    assert!(conditions.is_empty());

    let options = ResolverOptions {
        module_resolution: NormalizedModuleResolution::Node10,
        flags: ResolveFlags::empty(),
        custom_conditions: &[],
    };
    let conditions = get_conditions(&options, Some(ResolutionMode::ESNext));
    assert_eq!(conditions.by_resolution, ["import", "types", "node"]);

    let options = ResolverOptions {
        module_resolution: NormalizedModuleResolution::Node10,
        flags: ResolveFlags::empty(),
        custom_conditions: &["abc".to_string()],
    };
    let conditions = get_conditions(&options, Some(ResolutionMode::ESNext));
    assert_eq!(conditions.by_resolution, ["import", "types", "node"]);
    assert_eq!(conditions.custom, ["abc"]);
    let mut iter = conditions.into_iter();
    assert_eq!(iter.next(), Some("import"));
    assert_eq!(iter.next(), Some("types"));
    assert_eq!(iter.next(), Some("node"));
    assert_eq!(iter.next(), Some("abc"));
    assert_eq!(iter.next(), None);
}
