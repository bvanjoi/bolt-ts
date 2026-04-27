// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectCreate-errors.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

var e1 = Object.create(1);               // Error
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'null | object'.
var e2 = Object.create("string");        // Error
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'null | object'.
var e3 = Object.create(false);           // Error
//~^ ERROR: Argument of type 'boolean' is not assignable to parameter of type 'null | object'.
var e4 = Object.create(undefined);       // Error
//~^ ERROR: Argument of type 'undefined' is not assignable to parameter of type 'null | object'.

 
var e5 = Object.create(1, {});           // Error
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'null | object'.
var e6 = Object.create("string", {});    // Error
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'null | object'.
var e7 = Object.create(false, {});       // Error
//~^ ERROR: Argument of type 'boolean' is not assignable to parameter of type 'null | object'.
var e8 = Object.create(undefined, {});   // Error
//~^ ERROR: Argument of type 'undefined' is not assignable to parameter of type 'null | object'.
