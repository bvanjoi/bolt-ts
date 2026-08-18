// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reboundIdentifierOnImportAlias.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Foo {
    export var x = "hello";
}
namespace Bar {
    var Foo = 1;
    import F = Foo;
    //~^ ERROR: Module 'Foo' is hidden by a local declaration with the same name.
}