// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/ambiguousCallsWhereReturnTypesAgree.ts`, Apache-2.0 License
class TestClass {
  
  
  bar(x) {}
  
  
  foo(x) {
    this.bar(x);
  }
}
// should not error
class TestClass2 {
  
  
  bar(x) {
    return 0
  }
  
  
  foo(x) {
    return this.bar(x)
  }
}