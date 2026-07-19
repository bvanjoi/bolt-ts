// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyDeclareFunctionExprWithoutFormalType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

// these should be errors for implicit any parameter
var lambda = (l1) => { };       // Error at "l1"
//~^ ERROR: Parameter 'l1' implicitly has an 'any' type.
var lambd2 = (ll1, ll2: string) => { }  // Error at "ll1"
//~^ ERROR: Parameter 'll1' implicitly has an 'any' type.
var lamda3 = function myLambda3(myParam) { }
//~^ ERROR: Parameter 'myParam' implicitly has an 'any' type.
var lamda4 = () => { return null };

// these should be error for implicit any return type
var lambda5 = function temp() { return null; }
var lambda6 = () => { return null; }
var lambda7 = function temp() { return undefined; }
var lambda8 = () => { return undefined; }

// this shouldn't be an error
var lambda9 = () => { return 5; }
var lambda10 = function temp1() { return 5; }

