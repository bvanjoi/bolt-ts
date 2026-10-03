// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParametersWithUnderscore.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedParameters

function f<_T, U>() { }
//~^ ERROR: 'U' is declared but its value is never read.

type T<_T, U> = { };
//~^ ERROR: 'U' is declared but its value is never read.

interface I<_T, U> { };
//~^ ERROR: 'U' is declared but its value is never read.

class C<_T, U> {
//~^ ERROR: 'U' is declared but its value is never read.
    public m<_V, W>() { }
//~^ ERROR: 'W' is declared but its value is never read.
};

let l = <_T, U>() => { };
//~^ ERROR: 'U' is declared but its value is never read.
