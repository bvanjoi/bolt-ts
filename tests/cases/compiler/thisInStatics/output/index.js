// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInStatics.ts`, Apache-2.0 License
class C {
  static f() {
    var y = this;
  }
  /*1*/static get x() {
    var y = this;
    /*2*/return y
  }
}