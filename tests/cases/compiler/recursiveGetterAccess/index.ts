// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveGetterAccess.ts`, Apache-2.0 License

class MyClass {
  get testProp() { return this.testProp; }
  }
   
  