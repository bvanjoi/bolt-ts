// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/classImplementsClass2.ts`, Apache-2.0 License

class A { foo(): number { return 1; } }
class C implements A {} // error
//~^ ERROR: Property 'foo' is missing.

class C2 extends A {
    foo() {
        return 1;
    }
}

var c: C;
var c2: C2;
c = c2;
c2 = c;
//~^ ERROR: Property 'foo' is missing.