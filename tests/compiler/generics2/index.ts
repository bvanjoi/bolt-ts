// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/generics2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A { a: string; }
interface B extends A { b: string; }
interface C extends B { c: string; }
interface G<T, U extends B> {
    x: T;
    y: U;
}


var v1: {
    x: { a: string; }
    y: { a: string; b: string; c: string };
}; // Ok


var v2: G<{ a: string }, C>;   // Ok, equivalent to G<A, C>
var v3: G<A, A>;               // Error, A not valid argument for U
//~^ ERROR: Type 'A' does not satisfy the constraint 'B'.
var v4: G<G<A, B>, C>;         // Ok
var v5: G<any, any>;           // Error, any does not satisfy constraint B
var v6: G<any>;                // Error, wrong number of arguments
//~^ ERROR: Generic type 'G<T, U>' requires 2 type arguments.
var v7: G;                     // Error, no type arguments
//~^ ERROR: Generic type 'G<T, U>' requires 2 type arguments.

