// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeArgumentInferenceOrdering.ts`, Apache-2.0 License

class C {
  y: I;
}

interface I {
  x(): Goo;
}

interface Goo {
  p: string;
}

function foo<T>(f: { y: T }): T { return null }
var x = foo(new C()).x; // was Error that property x does not exist on type {}
let goo = x();
let p: number = goo.p;
//~^ ERROR: Type 'string' is not assignable to type 'number'. 