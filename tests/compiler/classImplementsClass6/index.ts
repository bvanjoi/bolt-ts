// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classImplementsClass6.ts`, Apache-2.0 License

class A {
    static bar(): string {
        return "";
    }
    foo(): number { return 1; }
}
class C implements A {

    foo() {
        return 1;
    }
}

class C2 extends A {}

declare var c: C;
declare var c2: C2;
c = c2;
c2 = c;
c.bar(); // error
//~^ ERROR: Property 'bar' does not exist on type 'C'.
c2.bar(); // should error
//~^ ERROR: Property 'bar' does not exist on type 'C2'.
