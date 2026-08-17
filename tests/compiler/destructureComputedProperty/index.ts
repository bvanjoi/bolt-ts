// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructureComputedProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare const ab: { n: number } | { n: string };
const nameN = "n";
const { [nameN]: n } = ab;

class C { private p: number; }
//~^ ERROR: Property 'p' has no initializer and is not definitely assigned in the constructor.
const nameP = "p";
const { "p": p0 } = new C();
//~^ ERROR: Property 'p' is private and only accessible within class 'C'.
const { ["p"]: p1 } = new C();
//~^ ERROR: Property 'p' is private and only accessible within class 'C'.
const { [nameP]: p2 } = new C();
//~^ ERROR: Property 'p' is private and only accessible within class 'C'.
const { p: p3 } = new C();
//~^ ERROR: Property 'p' is private and only accessible within class 'C'.
