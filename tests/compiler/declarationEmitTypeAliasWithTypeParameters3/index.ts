// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitTypeAliasWithTypeParameters3.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: declaration

type Foo<T> = {
    foo<U>(): Foo<U>
};
function bar() {
    return {} as Foo<number>;
}