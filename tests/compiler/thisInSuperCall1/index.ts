// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/thisInSuperCall1.ts`, Apache-2.0 License

class Base { 
    constructor(a: any) {}
}

class Foo extends Base {
    constructor(public x: number) {
        super(this);
        //~^ ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.
    }
}
