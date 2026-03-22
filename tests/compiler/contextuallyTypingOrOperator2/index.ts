// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextuallyTypingOrOperator2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var v: { a: (_: string) => number } = { a: s => s.length } || { a: s => 1 };
//~^ ERROR: This kind of expression is always truthy.
var v2 = (s: string) => s.length || function (s) { s.aaa };
