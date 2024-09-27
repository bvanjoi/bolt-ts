use rts_compiler::eval_and_emit;

fn main() {
    let cwd = project_root::get_project_root().unwrap();
    let p = cwd.join("tests/cases/compiler/bom-utf16le.ts");
    eval_and_emit(p);
}
