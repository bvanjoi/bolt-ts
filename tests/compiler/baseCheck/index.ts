// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/baseCheck.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C { constructor(x: number, y: number) { } }
class ELoc extends C {
    constructor(x: number) {
        super(0, x);
    }
}
class ELocVar extends C {  
    constructor(x: number) {
        super(0, loc);
        //~^ ERROR: Cannot find name 'loc'.
    }

    m() {
        var loc=10;
    }
}

class D extends C { constructor(public z: number) { super(this.z) }  } // too few params
//~^ ERROR: Expected 2 arguments, but got 1.
//~| ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.
class E extends C { constructor(public z: number) { super(0, this.z) } }
//~^ ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.
class F extends C { constructor(public z: number) { super("hello", this.z) } } // first param type
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
//~| ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.
//~| ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.

function f() {
    if (x<10) {
      //~^ ERROR: Cannot find name 'x'.
      x=11;
      //~^ ERROR: Cannot find name 'x'.
    }
    else {
        x=12;
      //~^ ERROR: Cannot find name 'x'.
    }
}
