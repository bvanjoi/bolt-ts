// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/enumNumbering1.ts`, Apache-2.0 License

enum Test {
  A,
  B,
  C = Math.floor(Math.random() * 1000),
  D = 10,
  E // Error but shouldn't be
}
