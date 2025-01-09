// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/ambientModuleWithClassDeclarationWithExtends.ts`, Apache-2.0 License

declare module foo {
  class A { }
  class B extends A { }
}