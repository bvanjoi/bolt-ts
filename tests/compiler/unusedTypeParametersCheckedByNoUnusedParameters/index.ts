// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParametersCheckedByNoUnusedParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedParameters

function f<T>() { }
//~^ ERROR: 'T' is declared but its value is never read.

type T<T> = { };
//~^ ERROR: 'T' is declared but its value is never read.

interface I<T> { };
//~^ ERROR: 'T' is declared but its value is never read.

class C<T> {
//~^ ERROR: 'T' is declared but its value is never read.
    public m<V>() { }
    //~^ ERROR: 'V' is declared but its value is never read.
};

let l = <T>() => { };
//~^ ERROR: 'T' is declared but its value is never read.
