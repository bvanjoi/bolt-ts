// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/excessPropertyErrorsSuppressed.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x: { a: string } = { a: "hello", b: 42 };  // No error
//~^ ERROR: Object literal may only specify known properties, and 'b' does not exist in type '{ a: string; }'.