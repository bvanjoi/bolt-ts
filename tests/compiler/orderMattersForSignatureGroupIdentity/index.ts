// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/orderMattersForSignatureGroupIdentity.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface A {
    (x: { s: string }): string
    (x: { n: number }): number
}

interface B {
    (x: { s: string }): string
    (x: { n: number }): number
}

interface C {
    (x: { n: number }): number
    (x: { s: string }): string
}

declare var v: A;
declare var v: B;

v({ s: "", n: 0 }).toLowerCase();
//~^ ERROR: No overload matches this call.
//~| ERROR: Property 'toLowerCase' does not exist on type 'never'.

declare var w: A;
declare var w: C;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'w' must be of type 'A', but here has type 'C'.

w({ s: "", n: 0 }).toLowerCase();
//~^ ERROR: No overload matches this call.
//~| ERROR: Property 'toLowerCase' does not exist on type 'never'.