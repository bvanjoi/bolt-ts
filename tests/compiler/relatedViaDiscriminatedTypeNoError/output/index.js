// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/relatedViaDiscriminatedTypeNoError.ts`, Apache-2.0 License
class Model {
  constructor(flag) {
    this.flag = flag}
}
class A {
  constructor(model) {
    this.model = model}
}
class B extends A {}