// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/blockScopedSameNameFunctionDeclarationES5.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

function foo(a: number) {
    if (a === 1) {
        function foo() { } // duplicate function
    //~[target=ES5]^ ERROR: Function declarations are not allowed inside blocks in strict mode when targeting 'ES5'.
        foo(); 
        foo(10); // not ok
//~^ ERROR: Expected 0 arguments, but got 1.
    }
    else {
        function foo() { } // duplicate function
    //~[target=ES5]^ ERROR: Function declarations are not allowed inside blocks in strict mode when targeting 'ES5'.
        foo(); 
        foo(10); // not ok
//~^ ERROR: Expected 0 arguments, but got 1.
    }
    foo(10); // not ok
    foo(); 
//~^ ERROR: Expected 1 arguments, but got 0.
}
foo(10);
foo(); // not ok - needs number
//~^ ERROR: Expected 1 arguments, but got 0.
