// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTupleTypeParameterReadonly.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

declare function each<T extends ReadonlyArray<any>>(cases: ReadonlyArray<T>): (fn: (...args: T) => any) => void;

const cases = [
    [1, '1'],
    [2, '2'],
] as const;

const eacher = each(cases);

eacher((a, b) => {
  //~^ ERROR: Argument of type '(a: 1 | 2, b: "1" | "2") => void' is not assignable to parameter of type '(...args: readonly [1, "1"] | readonly [2, "2"]) => any'.
    a;
    b;
});

// TODO: https://github.com/microsoft/TypeScript/issues/53255
eacher((...args) => {
    const [a, b] = args;
    a;
    b;
});
