// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/chainedAssignmentChecking.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class X {
  constructor(public z) { }
  a: number;
}

class Y {
  constructor(public z) { }
  a: number;
  b: string;
}

class Z {
  z: any;
  c: string;
}

var c1 = new X(3);
var c2 = new Y(5);
var c3 = new Z();

c1 = c2 = c3; // Should be error
//~^ ERROR: Property 'a' is missing.
//~| ERROR: Property 'a' is missing.
//~| ERROR: Property 'b' is missing.
