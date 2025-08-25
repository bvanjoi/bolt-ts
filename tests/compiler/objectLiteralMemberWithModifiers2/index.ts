// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/objectLiteralMemberWithModifiers2.ts`, Apache-2.0 License

var v = { public get foo() { } }
//~^ ERROR: Modifier cannot be used here.