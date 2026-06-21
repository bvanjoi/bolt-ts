// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/shadowingViaLocalValueOrBindingElement.ts`, Apache-2.0 License

//@compiler-options: target=es2015

if (true) {
    let x;
    if (true) {
        var x = 0; // Error
        //~^ ERROR: Cannot initialize outer scoped variable 'x' in the same scope as block scoped declaration 'x'.
        var { x = 0 } = { x: 0 }; // Error
        //~^ ERROR: Cannot initialize outer scoped variable 'x' in the same scope as block scoped declaration 'x'.
        var { x: x = 0 } = { x: 0 }; // Error
        //~^ ERROR: Cannot initialize outer scoped variable 'x' in the same scope as block scoped declaration 'x'.
        var { x } = { x: 0 }; // Error
        //~^ ERROR: Cannot initialize outer scoped variable 'x' in the same scope as block scoped declaration 'x'.
        var { x: x } = { x: 0 }; // Error
        //~^ ERROR: Cannot initialize outer scoped variable 'x' in the same scope as block scoped declaration 'x'.
        var [x] = [0];
        //~^ ERROR: Cannot initialize outer scoped variable 'x' in the same scope as block scoped declaration 'x'.
    }
}