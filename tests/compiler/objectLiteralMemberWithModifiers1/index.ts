// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/objectLiteralMemberWithModifiers1.ts`, Apache-2.0 License

var v = { public foo() { } }
//~^ ERROR: Modifier cannot be used here.
var b = { async foo() { } }