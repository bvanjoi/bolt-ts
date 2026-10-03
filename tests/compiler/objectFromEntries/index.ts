// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectFromEntries.ts`, Apache-2.0 License

//@compiler-options: target=es2019

const o = Object.fromEntries([['a', 1], ['b', 2], ['c', 3]]);
const o2 = Object.fromEntries(new URLSearchParams());
const o3 = Object.fromEntries(new Map([[Symbol("key"), "value"]]));

const frozenArray = Object.freeze([['a', 1], ['b', 2], ['c', 3]]);
const o4 = Object.fromEntries(frozenArray);

const frozenArray2: readonly [string, number][] = Object.freeze([['a', 1], ['b', 2], ['c', 3]]);
const o5 = Object.fromEntries(frozenArray2);
