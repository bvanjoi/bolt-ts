// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/generics1NoError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

interface A { a: string; }
interface B extends A { b: string; }
interface C extends B { c: string; }
interface G<T, U extends B> {
    x: T;
    y: U;
}
var v1: G<A, C>;               // Ok
var v2: G<{ a: string }, C>;   // Ok, equivalent to G<A, C>
var v4: G<G<A, B>, C>;         // Ok