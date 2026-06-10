// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeWithAsClauseAndLateBoundProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2020

declare let tgt2: number[];
declare let src2: { [K in keyof number[] as Exclude<K, "length">]: (number[])[K] };
tgt2 = src2; // Should error
//~^ ERROR: Property 'length' is missing.
