// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/propertyOverridingPrototype.ts`, Apache-2.0 License

class Base {
    foo() {
    }
}

class Derived extends Base {
    foo: () => { };
    //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
}

