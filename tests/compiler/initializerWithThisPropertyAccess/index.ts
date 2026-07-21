// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/initializerWithThisPropertyAccess.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration


class A {
    a: number;
    b = this.a;  // Error
    //~^ ERROR: Property 'a' is used before its initialization.
    c = () => this.a;
    d = (new A()).a;
    constructor() {
        this.a = 1;
    }
}

class B extends A {
    x = this.a;
}

class C {
    a!: number;
    b = this.a;
}

// Repro from #37979

class Foo {
    private bar: Bar;
    readonly barProp = this.bar.prop;
    //~^ ERROR: Property 'bar' is used before its initialization.
    constructor() {
        this.bar = new Bar();
    }
}

class Bar {
    readonly prop = false;
}
