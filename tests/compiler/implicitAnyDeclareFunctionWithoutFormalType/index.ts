// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyDeclareFunctionWithoutFormalType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

function foo(x) { };
//~^ ERROR: Parameter 'x' implicitly has an 'any' type.
function bar(x: number, y) { };  // error at "y"; no error at "x"
//~^ ERROR: Parameter 'y' implicitly has an 'any' type.
function func2(a, b, c) { };     // error at "a,b,c"
//~^ ERROR: Parameter 'a' implicitly has an 'any' type.
//~| ERROR: Parameter 'b' implicitly has an 'any' type.
//~| ERROR: Parameter 'c' implicitly has an 'any' type.
function func3(...args) { };     // error at "args" 
//~^ ERROR: Rest parameter 'args' implicitly has an 'any[]' type.
function func4(z= null, w= undefined) { };  // error at "z,w"

// these shouldn't be errors
function noError1(x= 3, y= 2) { };
function noError2(x: number, y: string) { };
