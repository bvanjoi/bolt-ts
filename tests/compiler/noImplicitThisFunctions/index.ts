// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitThisFunctions.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noImplicitThis

function f1(x) {
    // implicit any is still allowed
    return x + 1;
}

function f2(y: number) {
    // ok: no reference to this
    return y + 1;
}

function f3(z: number): number {
    // error: this is implicitly any
    return this.a + z;
    //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
}

// error: `this` is `window`, but is still of type `any`
let f4: (b: number) => number = b => this.c + b;
//~^ ERROR: The containing arrow function captures the global value of 'this'.
//~| ERROR: The containing arrow function captures the global value of 'this'.
let f5 = () => () => this;
//~^ ERROR: The containing arrow function captures the global value of 'this'.

let f6 = function() { return () => this; };
//~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
let f7 = function() { return function() { return this } };
//~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
