// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/blockScopedFunctionDeclarationStrictES5.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

"use strict";
if (true) {
    function foo() { } // Error to declare function in block scope
  //~[target=ES5]^ ERROR: Function declarations are not allowed inside blocks in strict mode when targeting 'ES5'.
    foo(); // This call should be ok
}
foo(); // Error to find name foo
//~^ ERROR: Cannot find name 'foo'.
