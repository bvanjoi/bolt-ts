// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/thisInSuperCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Base { 
    constructor(x: any) {}
}

class Foo extends Base {
    constructor() {
        super(this); // error: "super" has to be called before "this" accessing
        //~^ ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.
    }
}

class Foo2 extends Base {
    public p = 0;
    constructor() {
        super(this); // error
        //~^ ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.
    }
}

class Foo3 extends Base {
    constructor(public p) {
        super(this); // error
        //~^ ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.
    }
}