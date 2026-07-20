// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitExpressionInExtends2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

class C<T, U> {
    x: T;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
    y: U;
    //~^ ERROR: Property 'y' has no initializer and is not definitely assigned in the constructor.
}

function getClass<T>(c: T) {
    return C;
}

class MyClass extends getClass(2) <string, number> {
}
