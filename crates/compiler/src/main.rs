// use rts_compiler::parser::{Parser, ParserState};

use rts_compiler::eval_and_emit;

fn main() {
    let cwd = project_root::get_project_root().unwrap();
    let p = cwd.join("tests/cases/compiler/binaryArithmatic1.ts");
    eval_and_emit(p);
}
