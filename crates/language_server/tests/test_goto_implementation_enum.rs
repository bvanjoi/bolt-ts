mod utils;

use self::utils::compile_single_input;

#[test]
fn test_goto_implementation_enum_01() {
    // From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/fourslash/goToImplementationEnum_01.ts`, Apache-2.0 License
    const CODE: &str = r#"
enum [|Foo|] {
  Foo1 = function initializer() { return 5 } (),
  Foo2 = 6
}

Fo/*reference*/o;
"#;
    let parser_arena = bolt_ts_arena::bumpalo_herd::Herd::new();
    let type_arena = bolt_ts_arena::bumpalo::Bump::new();
    let mut t = compile_single_input(CODE, &parser_arena, &type_arena);
    let refer_to = t.verify_baseline_goto_implementation_by_marker("reference");
    assert_eq!(refer_to, ["Foo"]);
}
