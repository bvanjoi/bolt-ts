pub use bolt_ts_lib_reference::LIB_MAP;
pub use bolt_ts_lib_reference::LIBS;
pub use bolt_ts_lib_reference::bitset_of_lib;

pub fn get_default_lib_filename(
    options: &bolt_ts_config::NormalizedCompilerOptions,
) -> &'static str {
    use bolt_ts_config::Target::*;
    match options.target() {
        ESNext => "lib.esnext.full.d.ts",
        ES2025 => "lib.es2025.full.d.ts",
        ES2024 => "lib.es2024.full.d.ts",
        ES2023 => "lib.es2023.full.d.ts",
        ES2022 => "lib.es2022.full.d.ts",
        ES2021 => "lib.es2021.full.d.ts",
        ES2020 => "lib.es2020.full.d.ts",
        ES2019 => "lib.es2019.full.d.ts",
        ES2018 => "lib.es2018.full.d.ts",
        ES2017 => "lib.es2017.full.d.ts",
        ES2016 => "lib.es2016.full.d.ts",
        ES2015 => "lib.es6.d.ts",
        ES5 => "lib.es5.d.ts",
        _ => "lib.d.ts",
    }
}
