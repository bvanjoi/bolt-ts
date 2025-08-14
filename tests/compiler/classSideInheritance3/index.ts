// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classSideInheritance3.ts`, Apache-2.0 License

class A {
  constructor(public x: string) {
  }
}
class B extends A {
  constructor(x: string, public data: string) {
      super(x);
  }
}
class C extends A {
  constructor(x: string) {
      super(x);
  }
}

var r1: typeof A = B; // error
//~^ ERROR: Type 'typeof B' is not assignable to type 'typeof A'.
var r2: new (x: string) => A = B; // error
//~^ ERROR: Type 'typeof B' is not assignable to type 'new (x: string) => A'.
var r3: typeof A = C; // ok