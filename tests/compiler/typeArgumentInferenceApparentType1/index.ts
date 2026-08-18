// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeArgumentInferenceApparentType1.ts`, Apache-2.0 License

//@compiler-options: target=es6

function method<T>(iterable: Iterable<T>): T {
    return;
    //~^ ERROR: Type 'undefined' is not assignable to type 'T'.
}

var res: string = method("test");