// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/mergedDeclarations2.ts`, Apache-2.0 License

enum Foo {
    b
}
enum Foo {
    a = b
}

namespace Foo {
    export var x = b
    //~^ ERROR: Cannot find name 'b'.
}