// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/staticMismatchBecauseOfPrototype.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A {
    n: number;
}
declare var A: {
    prototype: A;
    new(): A;
};

class B extends A {
    n = "";
    //~^ ERROR: Property 'n' in type 'B<B>' is not assignable to the same property in base type 'A'.
}