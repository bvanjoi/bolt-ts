// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/initializersInAmbientEnums.ts`, Apache-2.0 License

declare enum E {
  a = 10,
  b = a,
  e = 10 << 2 * 8,
}