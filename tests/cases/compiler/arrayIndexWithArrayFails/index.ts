// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/arrayIndexWithArrayFails.ts`, Apache-2.0 License

declare const arr1: (string | string[])[];
declare const arr2: number[];
const j = arr2[arr1[0]];
//~^ ERROR: Type 'string[]' cannot be used as an index type.
const i = arr1[arr2[0]]
const k: (string | string[]) = arr1[arr2[0]]
