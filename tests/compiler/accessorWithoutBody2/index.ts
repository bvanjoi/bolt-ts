// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/accessorWithoutBody2.ts`, Apache-2.0 License

//@compiler-options: target=ES5
var v = { set foo(a) }
//~^ ERROR: Expected '{'.