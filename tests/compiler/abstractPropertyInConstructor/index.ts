// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/abstractPropertyInConstructor.ts`, Apache-2.0 License

//@compiler-options: target=es2015

abstract class AbstractClass {
    constructor(str: string, other: AbstractClass) {
        this.method(parseInt(str));
        let val = this.prop.toLowerCase();
        //~^ ERROR: Abstract property 'prop' in class 'AbstractClass' cannot be accessed in the constructor.
        //~| ERROR: Abstract property 'prop' in class 'AbstractClass' cannot be accessed in the constructor.

        if (!str) {
            this.prop = "Hello World";
            //~^ ERROR: Abstract property 'prop' in class 'AbstractClass' cannot be accessed in the constructor.
        }
        this.cb(str);
        //~^ ERROR: Abstract property 'cb' in class 'AbstractClass' cannot be accessed in the constructor.

        // OK, reference is inside function
        const innerFunction = () => {
            return this.prop;
        }

        // OK, references are to another instance
        other.cb(other.prop);
    }

    abstract prop: string;
    abstract cb: (s: string) => void;

    abstract method(num: number): void;

    other = this.prop;
    //~^ ERROR: Abstract property 'prop' in class 'AbstractClass' cannot be accessed in the constructor.
    //~| ERROR: Property 'prop' is used before its initialization.

    fn = () => this.prop;

    method2() {
        this.prop = this.prop + "!";
    }
}

abstract class DerivedAbstractClass extends AbstractClass {
    cb = (s: string) => {};

    constructor(str: string, other: AbstractClass, yetAnother: DerivedAbstractClass) {
        super(str, other);
        // there is no implementation of 'prop' in any base class
        this.cb(this.prop.toLowerCase());
    //~^ ERROR: Abstract property 'prop' in class 'AbstractClass' cannot be accessed in the constructor.

        this.method(1);

        // OK, references are to another instance
        other.cb(other.prop);
        yetAnother.cb(yetAnother.prop);
    }
}

class Implementation extends DerivedAbstractClass {
    prop = "";
    cb = (s: string) => {};

    constructor(str: string, other: AbstractClass, yetAnother: DerivedAbstractClass) {
        super(str, other, yetAnother);
        this.cb(this.prop);
    }

    method(n: number) {
        this.cb(this.prop + n);
    }
}

class User {
    constructor(a: AbstractClass) {
        a.prop;
        a.cb("hi");
        a.method(12);
        a.method2();
    }
}

abstract class C1 {
    abstract x: string;
    abstract y: string;

    constructor() {
        let self = this;                // ok
        let { x, y: y1 } = this;        // error
        //~^ ERROR: Abstract property 'x' in class 'C1' cannot be accessed in the constructor.
        //~| ERROR: Abstract property 'y' in class 'C1' cannot be accessed in the constructor.
        ({ x, y: y1, "y": y1 } = this); // error
        //~^ ERROR: Abstract property 'x' in class 'C1' cannot be accessed in the constructor.
        //~| ERROR: Abstract property 'y' in class 'C1' cannot be accessed in the constructor.
        //~| ERROR: Abstract property 'y' in class 'C1' cannot be accessed in the constructor.
    }
}

class C2 {
    x: string;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
    y: string;
    //~^ ERROR: Property 'y' has no initializer and is not definitely assigned in the constructor.

    constructor() {
        let self = this;                // ok
        let { x, y: y1 } = this;        // ok
        ({ x, y: y1, "y": y1 } = this); // ok
    }
}
