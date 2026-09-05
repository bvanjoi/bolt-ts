// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privacyCheckExportAssignmentOnExportedGenericInterface1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration

namespace Foo {
    export interface A<T> {
    }
}
interface Foo<T> {
}
var Foo: new () => Foo.A<Foo<string>>;
export = Foo;
//~^ ERROR: Variable 'Foo' is used before being assigned.