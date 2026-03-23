// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classImplementsClass4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
    private x = 1;
    foo(): number { return 1; }
}
class C implements A {
//~^ ERROR: Class 'C' incorrectly implements interface 'A'. 
    private x = 1;
    foo() {
        return 1;
    }
}

class C2 extends A {}

declare var c: C;
declare var c2: C2;
c = c2;
//~^ ERROR: Type 'C2' is not assignable to type 'C'.
c2 = c;
//~^ ERROR: Type 'C' is not assignable to type 'C2'.
