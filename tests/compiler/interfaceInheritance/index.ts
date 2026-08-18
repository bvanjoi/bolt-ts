// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceInheritance.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I1 {
    i1P1: number;
    i1P2(): void;
}

interface I2 extends I1 {
    i2P1: string;
}

interface I3 {
    i2P1: string; // has a member from i2P1, but not from I1
}

interface I4 {
	one: number;
}

interface I5 {
	one: string;
}

class C1 implements I2 { // should be an error - it doesn't implement the members of I1
  //~^ ERROR: Property 'i1P1' is missing.
  //~| ERROR: Property 'i1P2' is missing.
    public i2P1!: string;
}

declare var i2: I2;
var i1: I1;
declare var i3: I3;
i1 = i2;
i2 = i3; // should be an error - i3 does not implement the members of i1
//~^ ERROR: Property 'i1P1' is missing.
//~| ERROR: Property 'i1P2' is missing.

var c1: C1;

declare var i4: I4;
declare var i5: I5;

i4 = i5; // should be an error
//~^ ERROR: Type 'I5' is not assignable to type 'I4'.
i5 = i4; // should be an error
//~^ ERROR: Type 'I4' is not assignable to type 'I5'.
