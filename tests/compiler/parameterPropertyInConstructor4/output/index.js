// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterPropertyInConstructor4.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
export class C {
  constructor(a = [], b) {
    this.a = a
    }
}