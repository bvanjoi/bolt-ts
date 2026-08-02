// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/useBeforeDeclaration_destructuring.ts`, Apache-2.0 License

//@compiler-options: target=es2015

a;
//~^ ERROR: Variable 'a' is used before being assigned.
//~| ERROR: Block-scoped variable 'a' used before its declaration.
let {a, b = a} = {a: '', b: 1};
b;

function test({c, d = c}: Record<string, number>) {}

function test2({d = c, c}: Record<string, number>) {}
//~^ ERROR: Parameter 'd' cannot reference identifier 'c' declared after it.

function test3([c, d = c]: [string, string]) {}
function test4([c = d, d]: [string, string]) {}
//~^ ERROR: Parameter 'c' cannot reference identifier 'd' declared after it.
