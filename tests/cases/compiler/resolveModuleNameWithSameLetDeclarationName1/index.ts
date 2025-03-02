// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/resolveModuleNameWithSameLetDeclarationName1.ts`, Apache-2.0 License

declare module foo {

  interface Bar {

  }
}

let foo: foo.Bar; 