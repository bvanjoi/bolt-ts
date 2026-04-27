// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericClassesInModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace Foo {

    export class B<T>{ }

    export class A { }

}

var a = new Foo.B<Foo.A>();