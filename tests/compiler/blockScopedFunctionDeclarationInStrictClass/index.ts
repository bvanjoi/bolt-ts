// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/blockScopedFunctionDeclarationInStrictClass.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class c {
    method() {
        if (true) {
            function foo() { }
            //~[target=ES5]^ ERROR: Function declarations are not allowed inside blocks in strict mode when targeting 'ES5'. Class definitions are automatically in strict mode.
            foo(); // ok
        }
        foo(); // not ok
        //~^ ERROR: Cannot find name 'foo'.
    }
}