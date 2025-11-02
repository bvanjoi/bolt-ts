// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/overloadsInDifferentContainersDisagreeOnAmbient.ts`, Apache-2.0 License

declare namespace M {
    // Error because body is not ambient and this overload is
    export function f();
}

namespace M {
    export function f() { }
    //~^ ERROR: Overload signatures must all be ambient or non-ambient.
}