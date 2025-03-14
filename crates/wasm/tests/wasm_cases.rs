#![cfg(target_arch = "wasm32")]

use wasm_bindgen_test::*;

const ES5_CODE: &str = include_str!("../../lib/src/declared_file/lib.es5.d.ts");

fn compile(input: serde_json::Value) -> bolt_ts_compiler::Output {
    let cwd = "/".to_string();
    let mut files: indexmap::IndexMap<String, String> = serde_json::from_value(input).unwrap();
    let prev = files.insert("/lib.es5.d.ts".to_string(), ES5_CODE.to_string());
    assert!(prev.is_none());
    let prev = files.insert("/lib.es2015.d.ts".to_string(), "".to_string());
    assert!(prev.is_none());
    let prev = files.insert("/lib.es2015.core.d.ts".to_string(), "".to_string());
    assert!(prev.is_none());
    let prev = files.insert("/lib.es2015.symbol.d.ts".to_string(), "".to_string());
    assert!(prev.is_none());
    let prev = files.insert("/lib.es6.d.ts".to_string(), "".to_string());
    assert!(prev.is_none());
    bolt_ts_wasm::_compile(cwd, files)
}

fn success_compile(input: serde_json::Value) -> bolt_ts_compiler::Output {
    let output = compile(input);
    assert!(output.diags.is_empty());
    output
}

#[wasm_bindgen_test]
fn test_without_tsconfig_should_ok() {
    success_compile(serde_json::json!({
        "/index.ts": "
function add(a: number, b: number): number { 
    return a + b; 
}
add(1, 2);
"
    }));
}

#[wasm_bindgen_test]
fn test_builtin_atoms_should_init() {
    success_compile(serde_json::json!({
        "/index.ts": "
let a = undefined;
"
    }));
}
