// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/declarationMergeInModule1.ts`, Apache-2.0 License

declare module A {
  interface B {}
}

declare module A {
  function b(b: B): void
}