// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumWithBigint.ts`, Apache-2.0 License

enum E {
  0n = 0,
  //~^ ERROR: An enum member cannot have a numeric name.
}
