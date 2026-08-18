// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/classDeclaredBeforeClassFactory.ts`, Apache-2.0 License
class Derived extends makeBaseClass() {}
function makeBaseClass() {
  return class Base {};
}