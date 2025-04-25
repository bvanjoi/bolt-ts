// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classExtendsClauseClassNotReferringConstructor.ts`, Apache-2.0 License

class A { a: number; }
module Foo {
    var A = 1;
    class B extends A { b: string; }
    //~^ ERROR: Type 'number' is not a constructor function type.
}
