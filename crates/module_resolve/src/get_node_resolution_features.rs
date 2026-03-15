use bolt_ts_config::NormalizedModuleResolution;

use super::NodeResolutionFeatures;
use super::ResolverOptions;

pub(super) fn get_node_resolution_features(options: &ResolverOptions) -> NodeResolutionFeatures {
    let mut flags = NodeResolutionFeatures::empty();
    match options.module_resolution {
        NormalizedModuleResolution::Node16 => {
            flags = NodeResolutionFeatures::NODE16_DEFAULT;
        }
        NormalizedModuleResolution::NodeNext => {
            flags = NodeResolutionFeatures::NODE_NEXT_DEFAULT;
        }
        NormalizedModuleResolution::Bundler => {
            flags = NodeResolutionFeatures::BUNDLER_DEFAULT;
        }
        _ => {}
    }

    // match options.flags.resolve_package_json_exports() {
    //     true => {
    //         flags |= NodeResolutionFeatures::EXPORTS;
    //     }
    //     false => {
    //         flags &= !NodeResolutionFeatures::EXPORTS;
    //     }
    // }

    // match options.flags.resolve_package_json_imports() {
    //     true => {
    //         flags |= NodeResolutionFeatures::IMPORTS;
    //     }
    //     false => {
    //         flags &= !NodeResolutionFeatures::IMPORTS;
    //     }
    // }

    flags
}
