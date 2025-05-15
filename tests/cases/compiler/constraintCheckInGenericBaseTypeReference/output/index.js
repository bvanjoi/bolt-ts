// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constraintCheckInGenericBaseTypeReference.ts`, Apache-2.0 License
// No errors
class Constraint {
  method() {}
}
class GenericBase {
  items
}
class Derived extends GenericBase {}
class TypeArg {
  method() {
    Container.People.items;
  }
}
class Container {
  static People
}