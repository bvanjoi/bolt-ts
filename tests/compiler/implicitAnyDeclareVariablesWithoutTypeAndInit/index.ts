// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyDeclareVariablesWithoutTypeAndInit.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

// this should be an error
var x;                   // no error, control flow typed
var y;                   // error because captured
//~^ ERROR: Variable 'y' implicitly has type 'any' in some locations where its type cannot be determined.
declare var foo;         // error at "foo"
//~^ ERROR: Variable 'foo' implicitly has an 'any' type.
function func(k) { y };  // error at "k"
//~^ ERROR: Parameter 'k' implicitly has an 'any' type.
//~| ERROR: Variable 'y' implicitly has an 'any' type.
func(x);

// this shouldn't be an error
var bar = 3;            
var bar1: any;          
declare var bar2: any; 
var x1: any; var y1 = new x1;