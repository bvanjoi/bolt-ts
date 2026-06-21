// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceExtendsClassWithPrivate2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    public foo(x: any) { return x; }
    private x = 1;
}

interface I extends C {
    other(x: any): any;
}

class D extends C implements I { // error
//~^ ERROR: Class 'D' incorrectly extends base class 'C'.
//~| ERROR: Class 'D' incorrectly implements interface 'I'.
    public foo(x: any) { return x; }
    private x = 2;
    private y = 3;
    other(x: any) { return x; }
    bar() {}
} 

class D2 extends C implements I { // error
    public foo(x: any) { return x; }
    private x = "";
    //~^ ERROR: Property 'x' in type 'D2<D2>' is not assignable to the same property in base type 'C<D2>'.
    //~| ERROR: Property 'x' in type 'D2<D2>' is not assignable to the same property in base type 'I<D2>'.
    other(x: any) { return x; }
    bar() { }
} 