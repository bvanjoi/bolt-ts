// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitTypeAliasWithTypeParameters4.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: declaration

type Foo<T, Y> = {
    foo<U, J>(): Foo<U, J>
};
type SubFoo<R> = Foo<string, R>;

function foo() {
    return {} as SubFoo<number>;
}