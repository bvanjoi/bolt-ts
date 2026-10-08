// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitTypeofThisInClass.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration
class Foo {
  foo;
  bar//Public property 'bar' of exported class has or is using private name 'this'.(4031)
  ;
  baz//Public property 'baz' of exported class has or is using private
  ;
}