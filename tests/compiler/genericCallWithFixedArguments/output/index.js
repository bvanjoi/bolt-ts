// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCallWithFixedArguments.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class A {
  foo() {}
}
class B {
  bar() {}
}
function g(x) {}
g(7);