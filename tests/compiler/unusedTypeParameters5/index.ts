// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParameters5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class A<Dummy> {
    public x: Dummy;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}

var x: {
    new <T, U, K>(a: T): A<U>;
    //~^ ERROR: 'K' is declared but its value is never read.
}