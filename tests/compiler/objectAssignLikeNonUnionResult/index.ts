// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectAssignLikeNonUnionResult.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

interface Interface {
    field: number;
}
const defaultValue: Interface = { field: 1 };

declare function assign<T, U>(target: T, source: U): T & U;

// Displayed type: Interface & { field: number }
// Underlying type: Something else...
const data1 = assign(defaultValue, Date.now() > 3 ? { field: 2 } : {});

type ExtractRawComponent<T> = T extends { __raw: infer C } ? [L1: T, L2: C] : [R1: T];
type t1 = ExtractRawComponent<typeof data1>;

// ???
type Explode<T> = T extends { x: infer A } ? [A] : 'X';
// 'X' | [unknown] -- why?
type e1 = Explode<typeof data1>;