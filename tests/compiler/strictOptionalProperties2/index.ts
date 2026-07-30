// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/strictOptionalProperties2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: exactOptionalPropertyTypes
//@compiler-options: declaration

type T1 = { 0?: string | undefined } extends { 0?: string } ? true : false;  // false
type T2 = [(string | undefined)?] extends [string?] ? true : false;  // false
