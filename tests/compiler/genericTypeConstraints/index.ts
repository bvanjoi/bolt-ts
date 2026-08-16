// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericTypeConstraints.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo {
    fooMethod() {}
}

class FooExtended { }

class Bar<T extends Foo> { }

class BarExtended extends Bar<FooExtended> {
  //~^ ERROR: Type 'FooExtended' does not satisfy the constraint 'Foo'.
    constructor() {
        super();
    }
}