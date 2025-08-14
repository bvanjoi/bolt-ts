// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/chainedAssignment3.ts`, Apache-2.0 License

class A {
  id: number;
}

class B extends A {
  value: string;
}

var a: A;
var b: B;
a = b = null;
a = b = new B();
b = a = new B();

a.id = b.value = null;

// error cases
b = a = new A();
//~^ ERROR: Property 'value' is missing.
a = b = new A();
//~^ ERROR: Property 'value' is missing.

