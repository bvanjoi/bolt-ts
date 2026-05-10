// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/generics5.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A { a: string; }
interface B extends A { b: string; }

interface C extends B { c: string; }
interface G<T, U extends B> {


}

var v3: G<A, A>;               // Error, A not valid argument for U
//~^ ERROR: Type 'A' does not satisfy the constraint 'B'.

