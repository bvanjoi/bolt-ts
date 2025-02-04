// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/constructorArgsErrors2.ts`, Apache-2.0 License

class foo {
  constructor (public static a: number) { //~ ERROR: 'static' modifier cannot appear on a parameter.
  }
}
