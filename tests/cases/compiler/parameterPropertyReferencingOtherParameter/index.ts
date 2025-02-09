// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/parameterPropertyReferencingOtherParameter.ts`, Apache-2.0 License

class Foo {
  constructor(public x: number, public y: number = x) { }
}
