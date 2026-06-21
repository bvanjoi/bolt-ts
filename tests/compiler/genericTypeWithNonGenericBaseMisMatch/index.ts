// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericTypeWithNonGenericBaseMisMatch.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I {
	f: (a: { a: number }) => void
}
class X<T extends { a: string }> implements I {
	f(a: T): void { }
  //~^ ERROR: Property 'f' in type 'X<T, X>' is not assignable to the same property in base type 'I'.
}
var x = new X<{ a: string }>();
var i: I = x; // Should not be allowed -- type of 'f' is incompatible with 'I'
//~^ ERROR: Type 'X<{ a: string; }>' is not assignable to type 'I'.

