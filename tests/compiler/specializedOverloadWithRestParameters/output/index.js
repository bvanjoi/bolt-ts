// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/specializedOverloadWithRestParameters.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
class Base {
  foo() {}
}
class Derived1 extends Base {
  bar() {}
}// error

function f(tagName) {
  return null;
}// error

function g(tagName) {
  return null;
}