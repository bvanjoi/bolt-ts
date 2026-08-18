// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportEqualsClassNoRedeclarationError.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class SomeClass {
  static get someProp() {
    return 0;
  }
  static set someProp(value) {}
}
export default SomeClass;