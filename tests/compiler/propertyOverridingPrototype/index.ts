// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/propertyOverridingPrototype.ts`, Apache-2.0 License

class Base {
    foo() {
    }
}

class Derived extends Base {
    foo: () => { };
}

