// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/staticMemberOfClassAndPublicMemberOfAnotherClassAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface A {
    prop();
}
class B {
    public prop() { }
}
class C {
    public static prop() { }
}

var a: A = new B();
a = new C(); // error prop is missing
//~^ ERROR: Property 'prop' is missing.
a = B; // error prop is missing
//~^ ERROR: Property 'prop' is missing.
a = C;

var b: B = new C(); // error prop is missing
//~^ ERROR: Property 'prop' is missing.
b = B; // error prop is missing
//~^ ERROR: Property 'prop' is missing.
b = C;
b = a;

var c: C = new B();
c = B;
c = C;
c = a;