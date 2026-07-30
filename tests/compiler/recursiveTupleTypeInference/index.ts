// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveTupleTypeInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

export type A = "number" | "null" | A[];

export type F<T> = null extends T
    ? [F<NonNullable<T>>, "null"]
    : T extends number
    ? "number"
    : never;

export type G<T> = { [k in keyof T]: F<T[k]> };

interface K {
    b: number | null;
}

const gK: { [key in keyof K]: A } = { b: ["number", "null"] };

function foo<T>(g: G<T>): T {
    return {} as any;
}

foo(gK);
//~^ ERROR: Argument of type '{ b: A; }' is not assignable to parameter of type 'G<G<T>>'.
