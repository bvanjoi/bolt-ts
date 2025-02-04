// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/scopeCheckClassProperty.ts`, Apache-2.0 License

class C {
  constructor() {
    new A().p; // ok
  }
  public x = new A().p; // should also be ok
}
class A {
  public p = '';
}
