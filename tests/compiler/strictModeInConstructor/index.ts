// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/strictModeInConstructor.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
}

 

class B extends A {
    public s: number = 9;

    constructor () {
        "use strict";   // No error
        super();
    }
}

class C extends A {
    public s: number = 9;

    constructor () {
        super();            // No error
        "use strict";
    }
}

class D extends A {
    public s: number = 9;

    constructor () {
      //~^ ERROR: A 'super' call must be the first statement in the constructor to refer to 'super' or 'this' when a derived class contains initialized properties, parameter properties, or private identifiers.
        var x = 1; // No error
        var y = this.s; // Error
        //~^ ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.
        super();
        "use strict";
    }
}

class Bs extends A {
    public static s: number = 9;

    constructor () {
        "use strict";   // No error
        super();
    }
}

class Cs extends A {
    public static s: number = 9;

    constructor () {
        super();            // No error
        "use strict";
    }
}

class Ds extends A {
    public static s: number = 9;

    constructor () {
        var x = 1; // no Error
        super();
        "use strict";
    }
}