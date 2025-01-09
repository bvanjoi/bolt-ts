// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/typeOfThisInStatics.ts`, Apache-2.0 License

class C {
  static foo() {
      var r = this;
      var r0: C = this;
      var r1: {} = this;
  }
  static get x() {
      var r = this;
      var r0: C = this;
      var r1: {} = this;
      return 1;
  }
}
