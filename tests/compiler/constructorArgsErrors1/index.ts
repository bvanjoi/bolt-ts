// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constructorArgsErrors1.ts`, Apache-2.0 License

class foo {
  constructor (static a: number) { //~ ERROR: 'static' modifier cannot appear on a parameter.
  }
}