// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/numericIndexerConstraint2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo { foo() { } }
declare var x: { [index: string]: Foo; };
var a: { one: number; } = { one: 1 };
x = a;
//~^ ERROR: Type '{ one: number; }' is not assignable to type '{ [index: string]: Foo }'.