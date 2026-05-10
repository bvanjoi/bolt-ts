// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/duplicateVariablesWithAny.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// They should have to be the same even when one of the types is 'any'
var x: any;
var x = 2; //error
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type 'any', but here has type 'number'.

var y = "";
var y; //error
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'y' must be of type 'string', but here has type 'any'.

namespace N {
    var x: any;
    var x = 2; //error
    //~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type 'any', but here has type 'number'.

    var y = "";
    var y; //error
    //~^ ERROR: Subsequent variable declarations must have the same type. Variable 'y' must be of type 'string', but here has type 'any'.
}

var z: any;
var z; // ok