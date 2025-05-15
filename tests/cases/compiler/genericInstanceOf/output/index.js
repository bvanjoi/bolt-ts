
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericInstanceOf.ts`, Apache-2.0 License
class C {
  constructor(a, b) {
    this.a = a
    
    this.b = b}
  foo() {
    if (this.a instanceof this.b) {}
    
  }
}