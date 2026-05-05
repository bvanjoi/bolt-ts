// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/substitutionTypesCompareCorrectlyInRestrictiveInstances.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type UnionKeys<T> = T extends any ? keyof T : never;
type BugHelper<T, TAll> = T extends any ? Exclude<UnionKeys<TAll>, keyof T> : never
type Bug<TAll> =  BugHelper<TAll, TAll>
type Q = UnionKeys<{ a : any } | { b: any }>    // should be "a" | "b"
type R = Bug<{ a : any } | { b: any }>          // should be "a" | "b"

const q0: Q = 'a';
const q1: Q = 'b';

const q: Q = 'c';
//~^ ERROR: Type 'string' is not assignable to type 'Q'.

const r0: R = 'a';
const r1: R = 'b';
const r: R = 'c';
//~^ ERROR: Type 'string' is not assignable to type 'Q'.
