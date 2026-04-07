// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/thisInSuperCall3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Base { 
    constructor(a: any) {}
}

class Foo extends Base {
    public x: number = 0;

    constructor() {
        super(this);
        //~^ ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.
    }
}
