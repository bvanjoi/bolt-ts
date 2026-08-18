// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classExtendsClauseClassMergedWithModuleNotReferingConstructor.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
    a: number;
    //~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
}
namespace A {
    export var v: string;
}

namespace Foo {
    var A = 1;
    class B extends A {
      //~^ ERROR: Type 'number' is not a constructor function type.
        b: string;
    //~^ ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor.
    }
}