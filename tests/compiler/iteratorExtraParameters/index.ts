// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/iteratorExtraParameters.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/57130
const iter = {
    *[Symbol.iterator](_: number) {
        yield 0;
    }
};

declare function g(...args: any): any;

function* f() {
    for (const _ of iter);
    //~^ ERROR: Type '{ [iterator]: (_: number) => Generator<number, void, undefined>; }' must have a '[Symbol.iterator]()' method that returns an iterator.

    yield* iter;
    //~^ ERROR: Type '{ [iterator]: (_: number) => Generator<number, void, undefined>; }' must have a '[Symbol.iterator]()' method that returns an iterator.
    //~| ERROR: Type '{ [iterator]: (_: number) => Generator<number, void, undefined>; }' must have a '[Symbol.iterator]()' method that returns an iterator.

    [...iter]
    //~^ ERROR: Type '{ [iterator]: (_: number) => Generator<number, void, undefined>; }' must have a '[Symbol.iterator]()' method that returns an iterator.

    g(...iter);
    //~^ ERROR: Type '{ [iterator]: (_: number) => Generator<number, void, undefined>; }' must have a '[Symbol.iterator]()' method that returns an iterator.
}
