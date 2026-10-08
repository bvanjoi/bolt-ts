// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveGetterAccess.ts`, Apache-2.0 License

//@compiler-options: strict=false

class MyClass {
  get testProp() { return this.testProp; }
  }
   
  