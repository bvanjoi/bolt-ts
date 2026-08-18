// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileForClassWithMultipleBaseClasses.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
class A {
  foo() {}
}
class B {
  bar() {}
}
class D {
  baz() {}
  bat() {}
  foo() {}
  bar() {}
}