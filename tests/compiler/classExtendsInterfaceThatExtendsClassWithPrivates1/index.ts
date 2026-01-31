// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classExtendsInterfaceThatExtendsClassWithPrivates1.ts`, Apache-2.0 License

class C {
    public foo(x: any) { return x; }
    private x = 1;
}

interface I extends C {
    other(x: any): any;
}

class D2 implements I {
//~^ ERROR: Class 'D2' incorrectly implements interface 'I'.
    public foo(x: any) { return x }
    private x = 3;
    other(x: any) { return x }
} 