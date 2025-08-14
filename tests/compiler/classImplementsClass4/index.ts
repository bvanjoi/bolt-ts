// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/classImplementsClass4.ts`, Apache-2.0 License

class A {
    private x = 1;
    foo(): number { return 1; }
}
class C implements A {
//~^ ERROR: Property 'x' is missing.
    foo() {
        return 1;
    }
}

class C2 extends A {}

var c: C;
var c2: C2;
c = c2;
c2 = c;
//~^ ERROR: Property 'x' is missing.