// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/chainedCallsWithTypeParameterConstrainedToOtherTypeParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Chain<T extends A> {
    constructor(public value: T) { }
    then<S extends T>(cb: (x: T) => S): Chain<S> {
        return null;
    }
}

class A {
    x;
}
class B extends A {
    y;
}
class C extends B {
    z;
}

// Ok to go down the chain, but error to try to climb back up
(new Chain(new A)).then(a => new B).then(b => new C).then(c => new B).then(b => new A);
//~^ ERROR: Property 'z' is missing.
//~| ERROR: Type 'A' is not assignable to type 'S'.