// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/blockScopedFunctionDeclarationStrictES6.ts`, Apache-2.0 License

//@compiler-options: target=ES6

"use strict";
if (true) {
    function foo() { } // Allowed to declare block scope function
    foo(); // This call should be ok
}
foo(); // Cannot find name since foo is block scoped
//~^ ERROR: Cannot find name 'foo'.