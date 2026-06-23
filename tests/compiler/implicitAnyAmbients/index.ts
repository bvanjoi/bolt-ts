// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyAmbients.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

declare namespace m {
    var x; // error
    //~^ ERROR: Variable 'x' implicitly has an 'any' type.
    var y: any;

    function f(x); // error 
    //~^ ERROR: 'f', which lacks return-type annotation, implicitly has an 'any' return type.
    //~| ERROR: Parameter 'x' implicitly has an 'any' type.
    function f2(x: any); // error
    //~^ ERROR: 'f2', which lacks return-type annotation, implicitly has an 'any' return type.
    function f3(x: any): any;

    interface I {
        foo(); // error
        //~^ ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.
        foo2(x: any); // error
        //~^ ERROR: 'foo2', which lacks return-type annotation, implicitly has an 'any' return type.
        foo3(x: any): any;
    }

    class C {
        foo(); // error
        //~^ ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.
        foo2(x: any); // error
        //~^ ERROR: 'foo2', which lacks return-type annotation, implicitly has an 'any' return type.
        foo3(x: any): any;
    }

    namespace n {
        var y; // error
        //~^ ERROR: Variable 'y' implicitly has an 'any' type.
    }

    import m2 = n;
}