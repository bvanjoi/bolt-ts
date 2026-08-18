// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/inheritanceOfGenericConstructorMethod1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class A {}
class B extends A {}
var a = new A();
var b1 = new B();
var b2 = new B();
var b3 = new B();