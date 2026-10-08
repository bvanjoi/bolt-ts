// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superPropertyAccess1.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class C {
    public foo() { }
    public get x() {
        return 1;
    }

    public bar() { }
}

class D extends C {
    public foo() {
        super.bar();
        super.x;  // error
        //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
    }    

    constructor() {
        super();
        super.bar();
        super.x;  // error
        //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
    }

    public get y() {
        super.bar();
        super.x; // error
        //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
        return 1;
    }
}