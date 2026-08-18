// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/subSubClassCanAccessProtectedConstructor.ts`, Apache-2.0 License
class Base {
  constructor() {}
  instance1 = new Base();
}
class Subclass extends Base {
  instance1_1 = new Base();
  instance1_2 = new Subclass();
}
class SubclassOfSubclass extends Subclass {
  instance2_1 = new Base();
  instance2_2 = new Subclass();
  instance2_3 = new SubclassOfSubclass();
}