// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyNewExprLackConstructorSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

function Point() { this.x = 3; }
//~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
var x: any = new Point();  // error at "new"
//~^ ERROR: 'new' expression, whose target lacks a construct signature, implicitly has an 'any' type.