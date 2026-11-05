// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privacyCheckExportAssignmentOnExportedGenericInterface2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=amd
//@compiler-options: declaration

export = Foo;

interface Foo<T> {
}

function Foo<T>(array: T[]): Foo<T> {
    return undefined;
    //~^ ERROR: Type 'undefined' is not assignable to type 'Foo<T>'.
}

namespace Foo {
    export var x = "hello";
}
