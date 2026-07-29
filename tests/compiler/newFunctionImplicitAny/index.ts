// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/newFunctionImplicitAny.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

// No implicit any error given when newing a function (up for debate)

function Test() { }
var test = new Test();
//~^ ERROR: 'new' expression, whose target lacks a construct signature, implicitly has an 'any' type.