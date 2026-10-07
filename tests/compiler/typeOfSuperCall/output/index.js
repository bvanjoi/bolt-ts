// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeOfSuperCall.ts`, Apache-2.0 License
class C {}
class D extends C {
  constructor() {var x = super();}
}