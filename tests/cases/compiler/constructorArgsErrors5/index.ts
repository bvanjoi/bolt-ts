// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/constructorArgsErrors5.ts`, Apache-2.0 License

class foo {
  constructor (export a: number) { //~ ERROR: 'export' modifier cannot appear on a parameter.
  }
}
