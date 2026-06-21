// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/cannotIndexGenericWritingError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

function foo<T extends Record<string | symbol, any>>(target: T, p: string | symbol) {
    target[p] = ""; // error
    //~^ ERROR: Type 'T' is generic and can only be indexed for reading.
    //~| ERROR: Type 'T' is generic and can only be indexed for reading.
}

function foo2<T extends number[] & { [s: string]: number | string }>(target: T, p: string | number) {
    target[p] = 1; // error
    //~^ ERROR: Type 'T' is generic and can only be indexed for reading.
    target[1] = 1; // ok
}