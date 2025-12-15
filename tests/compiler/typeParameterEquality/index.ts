// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeParameterEquality.ts`, Apache-2.0 License

//@compiler-options: target=es6

class C {
    get x(): <T>(a: T) => T { return null; }
    set x(p: <U>(a: U) => U) {}
}