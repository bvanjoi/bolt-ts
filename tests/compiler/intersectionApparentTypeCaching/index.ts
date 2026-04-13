// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/intersectionApparentTypeCaching.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type TX<T extends any[] & object> = T["length"];
type T0<U extends any[] & object> = U;
type T1 = T0<string[]>;
