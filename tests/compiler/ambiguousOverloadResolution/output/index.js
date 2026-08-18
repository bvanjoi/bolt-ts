// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ambiguousOverloadResolution.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
class A {}
class B extends A {
  x;
}
var x;
var t = f(x, x);