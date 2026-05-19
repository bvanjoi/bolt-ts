// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeNoTypeNoCrash.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration

type T0<T> = ({[K in keyof T]}) extends ({[key in K]: T[K]}) ? number : never;
//~^ ERROR: Cannot find name 'K'.
//~| ERROR: Cannot find name 'K'.
