// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/parameterPropertyInitializerInInitializers.ts`, Apache-2.0 License
class Foo {
  constructor(x, y = x) {
    this.x = x
    
    this.y = y}
}