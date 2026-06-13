// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/substitutionTypeForNonGenericIndexedAccessType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

interface A {}

type B = A extends Record<'foo', string> ? A['foo'] : string; // no error

const b: B = 42;
//~^ ERROR: Type 'number' is not assignable to type 'string'.