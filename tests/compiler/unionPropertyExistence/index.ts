// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unionPropertyExistence.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A {
    inAll: string;
    notInB: string;
    notInC: string;
}

interface B {
    inAll: boolean;
    onlyInB: number;
    notInC: string;
}

interface C {
    inAll: number;
    notInB: string;
}

type AB = A | B;
type ABC = C | AB;

declare var ab: AB;
declare var abc: ABC;

declare const x: "foo" | "bar";
declare const bFoo: B | "foo";

x.nope();
//~^ ERROR: Property 'nope' does not exist on type '"foo" | "bar"'.
bFoo.onlyInB;
//~^ ERROR: Property 'onlyInB' does not exist on type 'B | "foo"'.
x.length; // Ok
bFoo.length;
//~^ ERROR: Property 'length' does not exist on type 'B | "foo"'.

ab.onlyInB;
//~^ ERROR: Property 'onlyInB' does not exist on type 'AB'.

ab.notInC; // Ok
abc.notInC;
//~^ ERROR: Property 'notInC' does not exist on type 'ABC'.
ab.notInB;
//~^ ERROR: Property 'notInB' does not exist on type 'AB'.
abc.notInB;
//~^ ERROR: Property 'notInB' does not exist on type 'ABC'.

abc.inAll; // Ok
abc.inNone;
//~^ ERROR: Property 'inNone' does not exist on type 'ABC'.
