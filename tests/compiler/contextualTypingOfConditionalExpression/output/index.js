// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTypingOfConditionalExpression.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var x = true ? (a) => (a.toExponential()) : (b) => (b.toFixed());
class A {
  foo;
}
class B extends A {
  bar;
}
class C extends A {
  baz;
}
var x2 = true ? (a) => (a.foo) : (b) => (b.foo);