// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/parameterPropertyInitializerInInitializers.ts`, Apache-2.0 License

class Foo {
  constructor(public x: number, public y: number = x) { }
}