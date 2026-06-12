// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeAliasInstantiationNoLeak1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Foo<T> = T | string | number;
type Bar<T> = Foo<T> | undefined;

declare let x1: Bar<"a">;
declare let x2: Bar<"b">;
