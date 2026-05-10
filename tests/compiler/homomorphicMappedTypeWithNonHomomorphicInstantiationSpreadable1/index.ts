// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/homomorphicMappedTypeWithNonHomomorphicInstantiationSpreadable1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type HandleOptions<O> = {
    [I in keyof O]: {
        value: O[I];
    };
};

declare function func1<
    T extends Record<PropertyKey, readonly any[]>,
>(fields: {
    [K in keyof T]: {
        label: string;
        options: [...HandleOptions<T[K]>];
    };
}): T;

const result = func1({
    prop: {
        label: "first",
        options: [
            {
                value: 123,
            },
            {
                value: "foo",
            },
        ],
    },
    other: {
        label: "second",
        options: [
            {
                value: "bar",
            },
            {
                value: true,
            },
        ],
    },
});