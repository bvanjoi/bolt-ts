// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeArgumentInferenceApparentType2.ts`, Apache-2.0 License

//@compiler-options: target=es6

function method<T>(iterable: Iterable<T>): T {
    function inner<U extends Iterable<T>>() {
        var u: U;
        var res: T = method(u);
        //~^ ERROR: Variable 'u' is used before being assigned.
        //~| ERROR: Variable 'u' is used before being assigned.
    }
    return;
    //~^ ERROR: Type 'undefined' is not assignable to type 'T'.
}