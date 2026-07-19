// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadsInDifferentContainersDisagreeOnAmbient.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare namespace M {
    // Error because body is not ambient and this overload is
    export function f();
}

namespace M {
    export function f() { }
    //~^ ERROR: Overload signatures must all be ambient or non-ambient.
    //~| ERROR: Overload signatures must all be ambient or non-ambient.
}
