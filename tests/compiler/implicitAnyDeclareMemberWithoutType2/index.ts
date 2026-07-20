// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyDeclareMemberWithoutType2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

// this should be an error
class C {
    public x = null;// error at "x"
    public x1: string  // no error
    //~^ ERROR: Property 'x1' has no initializer and is not definitely assigned in the constructor.

    constructor(c1, c2, c3: string) { }  // error at "c1, c2"
    //~^ ERROR: Parameter 'c1' implicitly has an 'any' type.
    //~| ERROR: Parameter 'c2' implicitly has an 'any' type.
    funcOfC(f1, f2, f3: number) { }     // error at "f1,f2"
    //~^ ERROR: Parameter 'f1' implicitly has an 'any' type.
    //~| ERROR: Parameter 'f2' implicitly has an 'any' type.
}

