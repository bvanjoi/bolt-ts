// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyForMethodParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

declare class A {
    private foo(a); // OK - ambient class and private method - no error
}

declare class B {
    public foo(a); // OK - ambient class and public method - error
    //~^ ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.
    //~| ERROR: Parameter 'a' implicitly has an 'any' type.
}

class C {
    private foo(a) { } // OK - non-ambient class and private method - error
    //~^ ERROR: Parameter 'a' implicitly has an 'any' type.
}
class D {
    public foo(a) { } // OK - non-ambient class and public method - error
    //~^ ERROR: Parameter 'a' implicitly has an 'any' type.
}
