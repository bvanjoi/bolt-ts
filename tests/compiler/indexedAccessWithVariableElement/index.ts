// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexedAccessWithVariableElement.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: noUncheckedIndexedAccess 

// repro from https://github.com/microsoft/TypeScript/issues/54420

declare const array1: [...number[], number]
const el1: number = array1[0]

declare const array2: [...number[], number]
const el2: number = array2[1]
//~^ ERROR: Type 'undefined | number' is not assignable to type 'number'.

declare const array3: [number, ...number[], number]
const el3: number = array3[1]

declare const array4: [number, ...number[], number]
const el4: number = array4[2]
//~^ ERROR: Type 'undefined | number' is not assignable to type 'number'.

const a0: [...number[], number] = [1];
const e0: number = a0[2];
//~^ ERROR: Type 'undefined | number' is not assignable to type 'number'.
const a1: [...number[], number] = [1, 2];
const a2: [...number[], number] = [];
//~^ ERROR: Type '[]' is not assignable to type '[...number[], number]'.
