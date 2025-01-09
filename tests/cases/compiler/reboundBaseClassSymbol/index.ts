// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/reboundBaseClassSymbol.ts`, Apache-2.0 License

interface A { a: number; }
module Foo {
    var A = 1;
    interface B extends A { b: string; } 
}