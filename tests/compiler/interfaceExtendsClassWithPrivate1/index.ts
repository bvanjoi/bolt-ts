// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceExtendsClassWithPrivate1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    public foo(x: any) { return x; }
    private x = 1;
}

interface I extends C {
    other(x: any): any;
}

class D extends C implements I {
    public foo(x: any) { return x; }
    other(x: any) { return x; }
    bar() { }
} 

var c: C;
declare var i: I;
declare var d: D;

c = i;
i = c; // error
//~^ ERROR: Property 'other' is missing

i = d;
d = i; // error
//~^ ERROR: Property 'bar' is missing

c = d;
d = c; // error
//~^ ERROR: Property 'other' is missing
//~| ERROR: Property 'bar' is missing