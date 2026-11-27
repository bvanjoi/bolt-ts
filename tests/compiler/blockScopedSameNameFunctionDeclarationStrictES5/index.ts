// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/blockScopedSameNameFunctionDeclarationStrictES5.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

"use strict";
function foo(a: number) {
    if (a === 1) {
        function foo() { } // Error to declare function in block scope
    //~[target=ES5]^ ERROR: Function declarations are not allowed inside blocks in strict mode when targeting 'ES5'.
        foo();
        foo(10); // not ok
//~^ ERROR: Expected 0 arguments, but got 1.
    }
    else {
        function foo() { } // Error to declare function in block scope
    //~[target=ES5]^ ERROR: Function declarations are not allowed inside blocks in strict mode when targeting 'ES5'.
        foo();
        foo(10); // not ok
//~^ ERROR: Expected 0 arguments, but got 1.
    }
    foo(10);
    foo(); // not ok - needs number
//~^ ERROR: Expected 1 arguments, but got 0.
}
foo(10);
foo(); // not ok - needs number
//~^ ERROR: Expected 1 arguments, but got 0.
