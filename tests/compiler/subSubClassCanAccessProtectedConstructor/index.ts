// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/subSubClassCanAccessProtectedConstructor.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Base {
    protected constructor() { }
    public instance1 = new Base(); // allowed
}

class Subclass extends Base {
    public instance1_1 = new Base(); // allowed
    public instance1_2 = new Subclass(); // allowed
}

class SubclassOfSubclass extends Subclass {
    public instance2_1 = new Base(); // allowed
    public instance2_2 = new Subclass(); // allowed
    public instance2_3 = new SubclassOfSubclass(); // allowed
}

