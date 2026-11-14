// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/subSubClassCanAccessProtectedConstructor.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Base {
  constructor() {}
  instance1 = new Base();// allowed
  
}
class Subclass extends Base {
  instance1_1 = new Base();// allowed
  
  instance1_2 = new Subclass();// allowed
  
}
class SubclassOfSubclass extends Subclass {
  instance2_1 = new Base();// allowed
  
  instance2_2 = new Subclass();// allowed
  
  instance2_3 = new SubclassOfSubclass();// allowed
  
}