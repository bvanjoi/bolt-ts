// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/returnInConstructor1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
    foo() { }
    constructor() {
        return;
    }
}

class B {
    foo() { }
    constructor() {
        return 1; // error
        //~^ ERROR: Type 'number' is not assignable to type 'B'.
        //~| ERROR: Return type of constructor signature must be assignable to the instance type of the class.
    }
}

class C {
    foo() { }
    constructor() {
        return this;
    }
}

class D {
    foo() { }
    constructor() {
        return "test"; // error
        //~^ ERROR: Type 'string' is not assignable to type 'D'.
        //~| ERROR: Return type of constructor signature must be assignable to the instance type of the class.
    }
}

class E {
    public foo: number;
    //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
    constructor() {
        return { foo: 1 };
    }
}

class F {
    public foo: string;
    //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
    constructor() {
        return { foo: 1 }; //error
        //~^ ERROR: Type 'number' is not assignable to type 'string'.
        //~| ERROR: Return type of constructor signature must be assignable to the instance type of the class.
    }
}

class G {
    private test: number;
    public test1() { }
    foo() { }
    constructor() {
        this.test = 2;
    }
}

class H extends F {
    constructor() {
        super();
        return new G(); //error
        //~^ ERROR: Type 'G' is not assignable to type 'H'.
        //~| ERROR: Return type of constructor signature must be assignable to the instance type of the class.
    }
}

class I extends G {
    constructor() {
        super();
        return new G();
    }
}

