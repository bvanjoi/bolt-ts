// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/thisConditionalOnMethodReturnOfGenericInstance.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class A {
  unmeasurableUsage;
}
class B extends A {
  method() {
    return '';
  }
}
class C extends B {
  marker;
}
var x = new C();
var y = x.method();