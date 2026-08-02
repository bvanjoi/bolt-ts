// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nameCollisions.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace T {
    var x = 2;

    namespace x { // error
      //~^ ERROR: Duplicate identifier 'x'.
        export class Bar {
            test: number;
            //~^ ERROR: Property 'test' has no initializer and is not definitely assigned in the constructor.
        }
    }

    namespace z {
        var t;
    }
    var z; // error
    //~^ ERROR: Duplicate identifier 'z'.

    namespace y {
      //~^ ERROR: A namespace declaration cannot be located prior to a class or function with which it is merged.
        var b;
    }

    class y { } // error

    var w;
    namespace w { } //ok

    var f;
    function f() { } //error
    //~^ ERROR: Duplicate identifier 'f'.

    function f2() { }
    var f2; // error
    //~^ ERROR: Duplicate identifier 'f2'.

    var i;
    interface i { } //ok

    class C { }
    //~^ ERROR: Class declaration cannot implement overload list for 'C'.
    function C() { } // error
    //~^ ERROR: Function with bodies can only merge with classes that are ambient.

    function C2() { }
    //~^ ERROR: Function with bodies can only merge with classes that are ambient.
    class C2 { } // error
    //~^ ERROR: Class declaration cannot implement overload list for 'C2'.

    function fi() { }
    interface fi { } // ok

    class cli { }
    interface cli { }

    interface cli2 { }
    class cli2 { }
}