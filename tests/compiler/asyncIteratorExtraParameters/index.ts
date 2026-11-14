// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/asyncIteratorExtraParameters.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/57130
const iter = {
    async *[Symbol.asyncIterator](_: number) {
        yield 0;
    }
};

declare function g(...args: any): any;

async function* f() {
    for await (const _ of iter);
    //~^ ERROR: Type '{ [asyncIterator]: (_: number) => Generator<number, void, undefined>; }' must have a '[Symbol.asyncIterator]()' method that returns an async iterator.

    yield* iter;
    //~^ ERROR: Type '{ [asyncIterator]: (_: number) => Generator<number, void, undefined>; }' must have a '[Symbol.asyncIterator]()' method that returns an async iterator.
    //~| ERROR: Type '{ [asyncIterator]: (_: number) => Generator<number, void, undefined>; }' must have a '[Symbol.asyncIterator]()' method that returns an async iterator.
}