// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/missingSelf.ts`, Apache-2.0 License
class CalcButton {
  a() {
    this.onClick();
  }
  onClick() {}
}
class CalcButton2 {
  b() {
    () => this.onClick();
  }
  onClick() {}
}
var c = new CalcButton();
c.a();
var c2 = new CalcButton2();
c2.b();