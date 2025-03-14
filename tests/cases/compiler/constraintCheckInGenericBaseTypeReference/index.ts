// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constraintCheckInGenericBaseTypeReference.ts`, Apache-2.0 License

// No errors
class Constraint {
  public method() { }
}
class GenericBase<T extends Constraint> {
  public items: any;
}
class Derived extends GenericBase<TypeArg> {

}
class TypeArg {
  public method() {
      Container.People.items;
  }
}

class Container {
  public static People: Derived
}