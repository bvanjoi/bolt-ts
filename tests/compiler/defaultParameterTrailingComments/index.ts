// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defaultParameterTrailingComments.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    constructor(defaultParam: boolean = false /* Emit only once*/) {}
}

function foo(defaultParam = 10 /*emit only once*/) {}