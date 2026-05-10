// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/numericIndexerConstraint1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo { foo() { } }
declare var x: { [index: string]: number; };
var result: Foo = x["one"]; // error
//~^ ERROR: Type 'number' is not assignable to type 'Foo'.
