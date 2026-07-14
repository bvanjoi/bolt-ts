// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericClassStaticMethod.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo<T> {
    static getFoo() {
    }
}

class Bar<T> extends Foo<T> {
    static getFoo() {
    }
}