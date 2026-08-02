// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCallbackInvokedInsideItsContainingFunction1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo<T, U>(x:T, y:U, f: (v: T) => U) {
    var r1 = f<number>(1);
    //~^ ERROR: Expected 0 type arguments, but got 1.
    var r2 = f(1);
    //~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'T'.
    var r3 = f<any>(null);
    //~^ ERROR: Expected 0 type arguments, but got 1.
    var r4 = f(null);
    //~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'T'.

    var r11 = f(x);
    var r21 = f<number>(x);
    //~^ ERROR: Expected 0 type arguments, but got 1.
    var r31 = f<any>(null);
    //~^ ERROR: Expected 0 type arguments, but got 1.
    var r41 = f(null);
    //~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'T'.

    var r12 = f(y);
    //~^ ERROR: Argument of type 'U' is not assignable to parameter of type 'T'.
    var r22 = f<number>(y);
    //~^ ERROR: Expected 0 type arguments, but got 1.
    var r32 = f<any>(null);
    //~^ ERROR: Expected 0 type arguments, but got 1.
    var r42 = f(null);
    //~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'T'.
}