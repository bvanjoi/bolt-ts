// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeInferenceToMappedType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare class Base<T> {
    someProp: T;
    method<U extends unknown[]>(x: { [K in keyof U]: U[K] }): Base<U>;
}

declare class Derived<T> extends Base<T> {
    method<V extends unknown[]>(x: { [K in keyof V]: V[K] }): Base<V>;
}