// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveInheritance3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
class C implements I {  //~ERROR: Property 'other' is missing.
    public foo(x: any) { return x; }
    private x = 1;
}

interface I extends C {
    other(x: any): any;
}