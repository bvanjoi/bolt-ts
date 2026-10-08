// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveBaseConstructorCreation1.ts`, Apache-2.0 License
class C1 {
  func(param) {}
}
class C2 extends C1 {}
var x = new C2();
// Valid
x.func(new C1());
x.func(new C2());