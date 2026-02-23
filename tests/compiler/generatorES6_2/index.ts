// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/generatorES6_2.ts`, Apache-2.0 License

//@compiler-options: target=es6
class C {
    public * foo() {
        yield 1
    }
}