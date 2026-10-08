// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParameters_infer.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedParameters

type Length<T> = T extends ArrayLike<infer U> ? number : never;
//~^ ERROR: 'U' is declared but its value is never read.
