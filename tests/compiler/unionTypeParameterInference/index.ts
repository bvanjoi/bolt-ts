// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unionTypeParameterInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo<T> { prop: T; }

declare function lift<U>(value: U | Foo<U>): Foo<U>;

function unlift<U>(value: U | Foo<U>): U {
    return lift(value).prop;
}