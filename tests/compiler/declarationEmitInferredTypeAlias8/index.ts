// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitInferredTypeAlias8.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

type Foo<T> = T | { x: Foo<T> };
var x: Foo<number[]>;

function returnSomeGlobalValue() {
    return x;
}