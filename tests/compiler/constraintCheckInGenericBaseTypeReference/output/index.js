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