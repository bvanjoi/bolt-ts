// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/augmentedTypesEnum.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// enum then var
enum e1111 { One } // error
var e1111 = 1; // error
//~^ ERROR: Enum declarations can only merge with namespace or other enum declarations.

// enum then function
enum e2 { One } // error
function e2() { } // error
//~^ ERROR: Enum declarations can only merge with namespace or other enum declarations.

enum e3 { One } // error
var e3 = () => { } // error
//~^ ERROR: Enum declarations can only merge with namespace or other enum declarations.

// enum then class
enum e4 { One } // error
class e4 { public foo() { } } // error
//~^ ERROR: Enum declarations can only merge with namespace or other enum declarations.

// enum then enum
enum e5 { One }
enum e5 { Two } // error
//~^ ERROR: In an enum with multiple declarations, only one declaration can omit an initializer for its first enum element.

enum e5a { One } // error
enum e5a { One } // error
//~^ ERROR: In an enum with multiple declarations, only one declaration can omit an initializer for its first enum element.
//~| ERROR: Duplicate identifier 'One'.

// enum then internal module
enum e6 { One } 
namespace e6 { } // ok

enum e6a { One }
namespace e6a { var y = 2; } // should be error

enum e6b { One }
namespace e6b { export var y = 2; } // should be error

// enum then import, messes with error reporting
//enum e7 { One }
//import e7 = require(''); // should be error