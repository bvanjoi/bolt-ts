// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/generativeRecursionWithTypeOf.ts`, Apache-2.0 License

class C<T> {
    static foo(x: number) { }
    type: T;
    //~^ ERROR: Property 'type' has no initializer and is not definitely assigned in the constructor.
}

namespace M {
    export function f(x: typeof C) {   
        return new x<typeof x>();     
    }
}
