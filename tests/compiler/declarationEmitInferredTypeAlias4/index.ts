// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitInferredTypeAlias4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

function f<A>() {
    type Foo<T> = T | { x: Foo<T> };
    var x: Foo<A[]>;
    return x;
    //~^ ERROR: Variable 'x' is used before being assigned.
}