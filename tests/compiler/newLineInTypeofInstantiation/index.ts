// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/newLineInTypeofInstantiation.ts`, Apache-2.0 License

interface Example {
  (a: number): typeof a

  <T>(): void
}
