pub use bolt_ts_lib_reference::DEFAULT_LIB_MAP;
pub use bolt_ts_lib_reference::DEFAULT_LIBS;
pub use bolt_ts_lib_reference::bitset_of_lib;

pub fn get_default_lib_filename(
    options: &bolt_ts_config::NormalizedCompilerOptions,
) -> &'static str {
    use bolt_ts_config::Target::*;
    match options.target() {
        ESNext => "lib.esnext.d.ts",
        ES2025 => "lib.es2025.d.ts",
        ES2024 => "lib.es2024.d.ts",
        ES2023 => "lib.es2023.d.ts",
        ES2022 => "lib.es2022.d.ts",
        ES2021 => "lib.es2021.d.ts",
        ES2020 => "lib.es2020.d.ts",
        ES2019 => "lib.es2019.d.ts",
        ES2018 => "lib.es2018.d.ts",
        ES2017 => "lib.es2017.d.ts",
        ES2016 => "lib.es2016.d.ts",
        ES2015 => "lib.es6.d.ts",
        ES5 => "lib.es5.d.ts",
        _ => "lib.d.ts",
    }
}
