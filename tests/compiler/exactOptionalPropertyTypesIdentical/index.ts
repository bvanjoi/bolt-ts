// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/exactOptionalPropertyTypesIdentical.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@compiler-options: exactOptionalPropertyTypes

export let a: <T>() => T extends {a?: string} ? 0 : 1 = null!;
export let b: <T>() => T extends {a?: string | undefined} ? 0 : 1 = a;
//~^ ERROR: Type '() => cond' is not assignable to type '() => cond'.

type A = {[x: string]: string; a?: string};