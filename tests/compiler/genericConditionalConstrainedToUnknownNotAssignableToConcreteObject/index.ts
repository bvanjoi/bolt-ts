// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericConditionalConstrainedToUnknownNotAssignableToConcreteObject.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A { x: number }

declare function isA(a: unknown): a is A;

type FunctionsObj<T> = {
    [K in keyof T]: () => unknown
}

function g<
    T extends FunctionsObj<T>,
    M extends keyof T
>(a2: ReturnType<T[M]>, x: A) {
    x = a2;
    //~^ ERROR: Type 'ReturnType' is not assignable to type 'A'.
}

// Original CFA report of the above issue

function g2<
    T extends FunctionsObj<T>,
    M extends keyof T
>(a2: ReturnType<T[M]>) {
    if (isA(a2)) {
        // a2 is not narrowed
        a2.x // error, but should be ok
    }
}
