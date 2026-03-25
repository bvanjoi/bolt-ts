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
    let mut t = compile_single_input(CODE);
    // TODO:
}
