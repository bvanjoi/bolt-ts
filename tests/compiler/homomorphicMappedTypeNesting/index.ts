// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/homomorphicMappedTypeIntersectionAssignability.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Box<T extends string> = { v: T };

type Test<T extends string[]> = T

type UnboxArray<T> = {
    [K in keyof T]: T[K] extends Box<infer R> ? R : never;
};

type Identity<T> = { [K in keyof T]: T[K] };

declare function fnBad<T extends Array<Box<string>>>(...args: T): Test<Identity<UnboxArray<T>>>;
