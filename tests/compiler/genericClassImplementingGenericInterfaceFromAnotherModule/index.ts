// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericClassImplementingGenericInterfaceFromAnotherModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace foo {
    export interface IFoo<T> { }
}
namespace bar {
    export class Foo<T> implements foo.IFoo<T> { }
}