use bolt_ts_fs::CachedFileSystem;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile(cwd: String, files: JsValue) -> JsValue {
    console_error_panic_hook::set_once();
    let files: indexmap::IndexMap<String, String> = serde_wasm_bindgen::from_value(files).unwrap();
    let output = _compile(cwd, files);
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

fn _compile(cwd: String, files: indexmap::IndexMap<String, String>) -> bolt_ts_compiler::Output {
    let mut atoms = bolt_ts_atom::AtomMap::new(1024);
    let root = std::path::PathBuf::from(cwd);
    let mut fs = bolt_ts_fs::MemoryFS::new(files, &mut atoms).unwrap();
    let tsconfig: bolt_ts_config::RawTsConfig = if let Ok(raw_tsconfig) =
        fs.read_file(std::path::Path::new("/tsconfig.json"), &mut atoms)
    {
        serde_json::from_str(atoms.get(raw_tsconfig)).unwrap()
    } else {
        Default::default()
    };
    let libs = bolt_ts_lib::LIB_ENTIRES
        .iter()
        .map(|(_, file)| root.join(file))
        .collect::<Vec<_>>();
    let tsconfig = tsconfig.normalize();
    let output = bolt_ts_compiler::eval_from_with_fs(root, tsconfig, libs, fs, atoms);
    output
}

#[cfg(target_arch = "wasm32")]
#[cfg(test)]
mod tests {
    use wasm_bindgen_test::*;

    const ES5_CODE: &str = include_str!("../../lib/src/declared_file/lib.es5.d.ts");

    fn compile(input: serde_json::Value) -> bolt_ts_compiler::Output {
        let cwd = "/".to_string();
        let files: indexmap::IndexMap<String, String> = serde_json::from_value(input).unwrap();
        super::_compile(cwd, files)
    }

    #[wasm_bindgen_test]
    fn test_without_tsconfig_should_ok() {
        let result = compile(serde_json::json!({
            "/lib.es5.d.ts": ES5_CODE,
            "/lib.es2015.d.ts": "",
            "/lib.es2015.core.d.ts": "",
            "/lib.es2015.symbol.d.ts": "",
            "/lib.es6.d.ts": "",
            "/index.ts": "
function add(a: number, b: number): number { 
    return a + b; 
}
add(1, 2);
"
        }));

        assert!(result.diags.is_empty())
    }
}
