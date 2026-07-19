// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/chainedAssignment3.ts`, Apache-2.0 License

class A {
  id: number;
  //~^ ERROR: Property 'id' has no initializer and is not definitely assigned in the constructor.
}

class B extends A {
  value: string;
  //~^ ERROR: Property 'value' has no initializer and is not definitely assigned in the constructor.
}

var a: A;
var b: B;
a = b = null;
//~^ ERROR: Type 'null' is not assignable to type 'B'.
//~| ERROR: Type 'null' is not assignable to type 'A'.
a = b = new B();
b = a = new B();

a.id = b.value = null;
//~^ ERROR: Type 'null' is not assignable to type 'string'.
//~| ERROR: Type 'null' is not assignable to type 'number'.

// error cases
b = a = new A();
//~^ ERROR: Property 'value' is missing.
a = b = new A();
//~^ ERROR: Property 'value' is missing.

