// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeIndexedAccess.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type Pairs<T> = {
    [TKey in keyof T]: {
        key: TKey;
        value: T[TKey];
    };
};

type Pair<T> = Pairs<T>[keyof T];

type FooBar = {
    foo: string;
    bar: number;
};

// Error expected here
let pair1: Pair<FooBar> = {
//~^ ERROR: Type '{ key: "foo"; value: number; }' is not assignable to type '{ key: "foo"; value: string; } | { key: "bar"; value: number; }'.
    key: "foo",
    value: 3
};

// Error expected here
let pair2: Pairs<FooBar>[keyof FooBar] = {
//~^ ERROR: Type '{ key: "foo"; value: number; }' is not assignable to type '{ key: "foo"; value: string; } | { key: "bar"; value: number; }'.
    key: "foo",
    value: 3
};
