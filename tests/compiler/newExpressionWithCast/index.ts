// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/newExpressionWithCast.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

function Test() { }
// valid but error with noImplicitAny
var test = new Test();
//~^ ERROR: 'new' expression, whose target lacks a construct signature, implicitly has an 'any' type.

function Test2() { }
// parse error
var test2 = new <any>Test2();
//~^ ERROR: Expression expected.
//~| ERROR: Cannot find name 'any'.
//~| ERROR: Operator '>' cannot be applied to types 'boolean' and 'void'.

function Test3() { }
// valid with noImplicitAny
var test3 = new (<any>Test3)();

