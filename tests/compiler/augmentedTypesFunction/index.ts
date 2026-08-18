// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/augmentedTypesFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// function then var
function y1() { } // error
var y1 = 1; // error
//~^ ERROR: Duplicate identifier 'y1'.

// function then function
function y2() { } // error
//~^ ERROR: Duplicate function implementation.
function y2() { } // error
//~^ ERROR: Duplicate function implementation.

function y2a() { }  // error
var y2a = () => { } // error
//~^ ERROR: Duplicate identifier 'y2a'.

// function then class
function y3() { } // error
//~^ ERROR: Function with bodies can only merge with classes that are ambient.
class y3 { } // error
//~^ ERROR: Class declaration cannot implement overload list for 'y3'.

function y3a() { } // error
//~^ ERROR: Function with bodies can only merge with classes that are ambient.
class y3a { public foo() { } } // error
//~^ ERROR: Class declaration cannot implement overload list for 'y3a'.

// function then enum
function y4() { } // error
enum y4 { One } // error
//~^ ERROR: Enum declarations can only merge with namespace or other enum declarations.

// function then internal module
function y5() { }
namespace y5 { } // ok since module is not instantiated

function y5a() { }
namespace y5a { var y = 2; } // should be an error

function y5b() { }
namespace y5b { export var y = 3; } // should be an error

function y5c() { }
namespace y5c { export interface I { foo(): void } } // should be an error

// function then import, messes with other errors
//function y6() { }
//import y6 = require('');