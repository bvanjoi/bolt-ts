// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumsWithMultipleDeclarations2.ts`, Apache-2.0 License

enum E {
  A
}

enum E {
  B = 1
}

enum E {
  C
}