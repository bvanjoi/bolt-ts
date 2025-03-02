// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInStatics.ts`, Apache-2.0 License

class C {
  static f() {
      var y/*1*/ = this;
  }

  static get x() {
      var y/*2*/ = this;
      return y;
  }
}
