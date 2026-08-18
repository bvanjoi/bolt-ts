// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadOnGenericClassAndNonGenericClass.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class A {
  a;
}
class B {
  b;
}
class C {
  c;
}
class X {
  x;
}
class X1 {
  x;
}
class X2 {
  x;
}
function f(a) {}
var xs;
var t3 = f(xs);
var t3;