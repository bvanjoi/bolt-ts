// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/uniqueSymbolAllowsIndexInObjectWithIndexSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es6]


// https://github.com/Microsoft/TypeScript/issues/21962
export const SYM = Symbol('a unique symbol');

export interface I {
  [SYM]: 'sym';
  [x: string]: 'str';
}

let a: I = {[SYM]: 'sym'}; // Expect ok
let b: I = {[SYM]: 'str'}; // Expect error
//~^ ERROR: Type '"str"' is not assignable to type '"sym"'.
