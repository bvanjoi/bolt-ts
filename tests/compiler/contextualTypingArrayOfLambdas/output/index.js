// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTypingArrayOfLambdas.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class A {
  foo;
}
class B extends A {
  bar;
}
class C extends A {
  baz;
}
var xs = [(x) => {}, (x) => {}, (x) => {}];