#![cfg(target_arch = "wasm32")]

use wasm_bindgen_test::*;

fn compile(input: serde_json::Value) -> bolt_ts_compiler::Output {
    let mut files: indexmap::IndexMap<String, String> = serde_json::from_value(input).unwrap();
    const DEFAULT_LIB_DIR: &str = "/node_modules/typescript/lib/";
    let cwd = "/".to_string();
    bolt_ts_wasm::_compile(cwd, DEFAULT_LIB_DIR.to_string(), files)
}

fn success_compile(input: serde_json::Value) -> bolt_ts_compiler::Output {
    let output = compile(input);
    assert!(output.diags.is_empty());
    output
}

#[wasm_bindgen_test]
fn test_without_tsconfig() {
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
fn test_builtin_atoms() {
    success_compile(serde_json::json!({
        "/index.ts": "
let a = undefined;
"
    }));
}

#[wasm_bindgen_test]
fn test_target_options() {
    success_compile(serde_json::json!({
        "/index.ts": "
'abc'.includes('a');
",
        "/tsconfig.json": r#"
{
    "compilerOptions": {
        "target": "es2015"
    }
}
"#,
    }));
}

#[wasm_bindgen_test]
fn test_cross_module() {
    success_compile(serde_json::json!({
        "/index.ts": r#"
import { a } from "./a";
const b: number = a;
"#,
        "/a.ts": "export const a = 42;"
    }));
}
