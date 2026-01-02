// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/accessorWithoutBody1.ts`, Apache-2.0 License

//@compiler-options: target=ES5

var v = { get foo() }
//~^ ERROR: Expected '{'.