use bolt_ts_fs::CachedFileSystem;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile(cwd: String, default_lib_dir: String, files: JsValue) -> JsValue {
    console_error_panic_hook::set_once();
    let files: indexmap::IndexMap<String, String> = serde_wasm_bindgen::from_value(files).unwrap();
    let output = _compile(cwd, default_lib_dir, files);
    if output.diags.is_empty() {
        let mut result = indexmap::IndexMap::new();
        for (m, value) in output.output {
            let path = output.module_arena.get_path(m);
            let file_path = path.with_extension("js");
            result.insert(file_path.to_string_lossy().to_string(), value);
        }
        serde_wasm_bindgen::to_value(&result).unwrap()
    } else {
        let diags = output
            .diags
            .iter()
            .map(|diag| {
                let m = diag.inner.module_id();
                let path = output.module_arena.get_path(m);
                let path = path.to_string_lossy().to_string();
                let primary_label = diag
                    .inner
                    .labels()
                    .unwrap()
                    .find(|label| label.primary())
                    .expect("at least one primary label");
                let msg = if let Some(msg) = primary_label.label() {
                    msg.to_string()
                } else {
                    diag.inner.to_string()
                };
                let code = output.module_arena.get_content(diag.inner.module_id());
                let (start, end) =
                    bolt_ts_errors::miette_label_span_to_line_position(primary_label, code);

                (
                    path,
                    (start.line, start.column),
                    (end.line, end.column),
                    msg,
                )
            })
            .collect::<Vec<_>>();
        serde_wasm_bindgen::to_value(&diags).unwrap()
    }
}

pub fn _compile(
    cwd: String,
    default_lib_dir: String,
    mut files: indexmap::IndexMap<String, String>,
) -> bolt_ts_compiler::Output {
    let mut atoms = bolt_ts_compiler::init_atom();
    let root = std::path::PathBuf::from(cwd);
    add_default_libs(&mut files, &default_lib_dir);
    let default_lib_dir = std::path::PathBuf::from(default_lib_dir);
    let mut fs = bolt_ts_fs::MemoryFS::new(files, &mut atoms).unwrap();
    let tsconfig: bolt_ts_config::RawTsConfig = if let Ok(raw_tsconfig) =
        fs.read_file(std::path::Path::new("/tsconfig.json"), &mut atoms)
    {
        serde_json::from_str(atoms.get(raw_tsconfig)).unwrap()
    } else {
        Default::default()
    };
    let default_libs = bolt_ts_libs::DEFAULT_LIBS
        .iter()
        .map(|filename| default_lib_dir.join(filename))
        .collect::<Vec<_>>();
    let tsconfig = tsconfig.normalize();
    bolt_ts_compiler::eval_from_with_fs(root, &tsconfig, default_lib_dir, default_libs, fs, atoms)
}

fn add_default_libs(files: &mut indexmap::IndexMap<String, String>, default_lib_dir: &str) {
    macro_rules! add_default_lib_file {
        ($($name: literal),* $( , )?) => {
            $(
                let code = include_str!(concat!("../../libs/src/declared_file/", $name));
                let prev = files.insert(format!("{default_lib_dir}/{}", $name), code.to_string());
                assert!(prev.is_none());
            )*
        };
    }
    add_default_lib_file!(
        "lib.d.ts",
        "lib.decorators.d.ts",
        "lib.decorators.legacy.d.ts",
        "lib.dom.asynciterable.d.ts",
        "lib.dom.d.ts",
        "lib.dom.iterable.d.ts",
        "lib.es5.d.ts",
        "lib.es6.d.ts",
        "lib.es2015.collection.d.ts",
        "lib.es2015.core.d.ts",
        "lib.es2015.d.ts",
        "lib.es2015.generator.d.ts",
        "lib.es2015.iterable.d.ts",
        "lib.es2015.promise.d.ts",
        "lib.es2015.proxy.d.ts",
        "lib.es2015.reflect.d.ts",
        "lib.es2015.symbol.d.ts",
        "lib.es2015.symbol.wellknown.d.ts",
        "lib.es2016.array.include.d.ts",
        "lib.es2016.d.ts",
        "lib.es2016.full.d.ts",
        "lib.es2016.intl.d.ts",
        "lib.es2017.arraybuffer.d.ts",
        "lib.es2017.d.ts",
        "lib.es2017.date.d.ts",
        "lib.es2017.full.d.ts",
        "lib.es2017.intl.d.ts",
        "lib.es2017.object.d.ts",
        "lib.es2017.sharedmemory.d.ts",
        "lib.es2017.string.d.ts",
        "lib.es2017.typedarrays.d.ts",
        "lib.es2018.asyncgenerator.d.ts",
        "lib.es2018.asynciterable.d.ts",
        "lib.es2018.d.ts",
        "lib.es2018.full.d.ts",
        "lib.es2018.intl.d.ts",
        "lib.es2018.promise.d.ts",
        "lib.es2018.regexp.d.ts",
        "lib.es2019.array.d.ts",
        "lib.es2019.d.ts",
        "lib.es2019.full.d.ts",
        "lib.es2019.intl.d.ts",
        "lib.es2019.object.d.ts",
        "lib.es2019.string.d.ts",
        "lib.es2019.symbol.d.ts",
        "lib.es2020.bigint.d.ts",
        "lib.es2020.d.ts",
        "lib.es2020.date.d.ts",
        "lib.es2020.full.d.ts",
        "lib.es2020.intl.d.ts",
        "lib.es2020.number.d.ts",
        "lib.es2020.promise.d.ts",
        "lib.es2020.sharedmemory.d.ts",
        "lib.es2020.string.d.ts",
        "lib.es2020.symbol.wellknown.d.ts",
        "lib.es2021.d.ts",
        "lib.es2021.full.d.ts",
        "lib.es2021.intl.d.ts",
        "lib.es2021.promise.d.ts",
        "lib.es2021.string.d.ts",
        "lib.es2021.weakref.d.ts",
        "lib.es2022.array.d.ts",
        "lib.es2022.d.ts",
        "lib.es2022.error.d.ts",
        "lib.es2022.full.d.ts",
        "lib.es2022.intl.d.ts",
        "lib.es2022.object.d.ts",
        "lib.es2022.regexp.d.ts",
        "lib.es2022.string.d.ts",
        "lib.es2023.array.d.ts",
        "lib.es2023.collection.d.ts",
        "lib.es2023.d.ts",
        "lib.es2023.full.d.ts",
        "lib.es2023.intl.d.ts",
        "lib.es2024.arraybuffer.d.ts",
        "lib.es2024.collection.d.ts",
        "lib.es2024.d.ts",
        "lib.es2024.full.d.ts",
        "lib.es2024.object.d.ts",
        "lib.es2024.promise.d.ts",
        "lib.es2024.regexp.d.ts",
        "lib.es2024.sharedmemory.d.ts",
        "lib.es2024.string.d.ts",
        "lib.esnext.array.d.ts",
        "lib.esnext.collection.d.ts",
        "lib.esnext.d.ts",
        "lib.esnext.decorators.d.ts",
        "lib.esnext.disposable.d.ts",
        "lib.esnext.float16.d.ts",
        "lib.esnext.full.d.ts",
        "lib.esnext.intl.d.ts",
        "lib.esnext.iterator.d.ts",
        "lib.esnext.promise.d.ts",
        "lib.scripthost.d.ts",
        "lib.webworker.asynciterable.d.ts",
        "lib.webworker.d.ts",
        "lib.webworker.importscripts.d.ts",
        "lib.webworker.iterable.d.ts",
    );
}
