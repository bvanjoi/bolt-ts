// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classExpressionPropertyModifiers.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: lib=[es6]


const a = class Cat {
    declare [Symbol.toStringTag] = "uh";
    //~^ ERROR: Initializers are not allowed in ambient contexts.
    export foo = 1;
    //~^ ERROR: 'export' modifier cannot appear on class elements of this kind.
}
