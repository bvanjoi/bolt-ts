// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/accessorWithoutBody2.ts`, Apache-2.0 License

//@compiler-options: target=ES5
//@compiler-options: strict=false

var v = { set foo(a) }
//~^ ERROR: Expected '{'.