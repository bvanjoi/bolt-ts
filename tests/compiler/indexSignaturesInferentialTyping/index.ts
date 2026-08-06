// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexSignaturesInferentialTyping.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo<T>(items: { [index: number]: T }): T { return undefined; }
//~^ ERROR: Type 'undefined' is not assignable to type 'T'.
function bar<T>(items: { [index: string]: T }): T { return undefined; }
//~^ ERROR: Type 'undefined' is not assignable to type 'T'.

var x1 = foo({ 0: 0, 1: 1 });       // type should be number
var x2 = bar({ 0: 0, 1: 1 });
var x3 = bar({ zero: 0, one: 1 });  // type should be number
