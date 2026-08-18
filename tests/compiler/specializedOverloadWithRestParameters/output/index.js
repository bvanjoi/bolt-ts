// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/specializedOverloadWithRestParameters.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Base {
  foo() {}
}
class Derived1 extends Base {
  bar() {}
}
function f(tagName) {
  return null;
}
function g(tagName) {
  return null;
}