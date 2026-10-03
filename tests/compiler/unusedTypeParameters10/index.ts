// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParameters10.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

type Alias<T> = { };
//~^ ERROR: 'T' is declared but its value is never read.
type Alias2<T> = { x: T };
