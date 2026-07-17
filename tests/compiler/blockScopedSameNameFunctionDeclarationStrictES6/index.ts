// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/blockScopedSameNameFunctionDeclarationStrictES6.ts`, Apache-2.0 License

//@compiler-options: target=ES6

"use strict";
function foo(a: number) {
    if (a === 10) {
        function foo() { }
        foo();
        foo(10); // not ok
//~^ ERROR: Expected 0 arguments, but got 1.
    }
    else {
        function foo() { } 
        foo();
        foo(10); // not ok
//~^ ERROR: Expected 0 arguments, but got 1.
    }
    foo(10);
    foo(); // not ok
//~^ ERROR: Expected 1 arguments, but got 0.
}
foo(10);
foo(); // not ok - needs number
//~^ ERROR: Expected 1 arguments, but got 0.