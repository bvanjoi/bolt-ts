// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceExactOptionalProperties1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: exactOptionalPropertyTypes
//@compiler-options: noEmit

type Test1 = { prop?: never } extends { prop?: infer T } ? T : false; // never

const a: Test1 = 42;
//~^ ERROR: Type 'number' is not assignable to type 'never'.