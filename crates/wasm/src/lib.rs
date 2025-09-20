mod query;

use wasm_bindgen::prelude::*;

use crate::query::JsQuery;

#[wasm_bindgen]
pub fn compile(cwd: String, default_lib_dir: String, files: JsValue) -> JsQuery {
    console_error_panic_hook::set_once();
    let files: indexmap::IndexMap<String, String> = serde_wasm_bindgen::from_value(files).unwrap();
    let output = bolt_ts_compiler::eval_from_memory_path(cwd, default_lib_dir, files);
    JsQuery::new(output)
}
