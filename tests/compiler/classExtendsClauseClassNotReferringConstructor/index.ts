// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classExtendsClauseClassNotReferringConstructor.ts`, Apache-2.0 License

class A { a: number; }
//~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
module Foo {
    var A = 1;
    class B extends A { b: string; }
    //~^ ERROR: Type 'number' is not a constructor function type.
    //~| ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor.
}
