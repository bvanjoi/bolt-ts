// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/initializersInAmbientEnums.ts`, Apache-2.0 License

declare enum E {
  a = 10,
  b = a,
  e = 10 << 2 * 8,
}