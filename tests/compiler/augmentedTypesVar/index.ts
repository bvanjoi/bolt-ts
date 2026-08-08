// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/augmentedTypesVar.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// var then var
var x1 = 1;
var x1 = 2;

// var then function
var x2 = 1; // error
function x2() { } // error
//~^ ERROR: Duplicate identifier 'x2'.

var x3 = 1; 
var x3 = () => { } // error
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x3' must be of type 'number', but here has type '() => void'.

// var then class
var x4 = 1; // error
class x4 { } // error
//~^ ERROR: Duplicate identifier 'x4'.

var x4a = 1; // error
class x4a { public foo() { } } // error
//~^ ERROR: Duplicate identifier 'x4a'.

// var then enum
var x5 = 1;
enum x5 { One } // error
//~^ ERROR: Enum declarations can only merge with namespace or other enum declarations.

// var then module
var x6 = 1;
namespace x6 { } // ok since non-instantiated

var x6a = 1; // error
namespace x6a { var y = 2; } // error since instantiated
//~^ ERROR: Duplicate identifier 'x6a'.

var x6b = 1; // error
namespace x6b { export var y = 2; } // error
//~^ ERROR: Duplicate identifier 'x6b'.

// var then import, messes with other error reporting
//var x7 = 1;
//import x7 = require('');
