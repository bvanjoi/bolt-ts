use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn hello() -> String {
    return String::from("hello world");
}

#[wasm_bindgen]
pub fn eval(files: JsValue, root: String) -> String {
    // TODO:
    String::from("hello world")
}
