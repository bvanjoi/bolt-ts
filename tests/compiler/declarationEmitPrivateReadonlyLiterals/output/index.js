// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitPrivateReadonlyLiterals.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Foo {
  static A = 'a';
  B = 'b';
  static C = 42;
  D = 42;
}