// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/ambientModuleWithClassDeclarationWithExtends.ts`, Apache-2.0 License

declare module foo {
  class A { }
  class B extends A { }
}