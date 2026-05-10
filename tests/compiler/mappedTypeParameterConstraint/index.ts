// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/mappedTypeParameterConstraint.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type MyMap<T> = {[P in keyof T]: T[keyof T]};
function foo<U>(arg: U): MyMap<U> {
    return arg;
}