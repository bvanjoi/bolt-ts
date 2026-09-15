// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multiExtendsSplitInterfaces2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A {
	a: number;
}

interface I extends A {
	i1: number;
}

interface B {
	b: number;
}

interface I extends B {
	i2: number;
}

var i: I;

var a = i.a;
//~^ ERROR: Variable 'i' is used before being assigned.
var i1 = i.i1;
//~^ ERROR: Variable 'i' is used before being assigned.
var b = i.b;
//~^ ERROR: Variable 'i' is used before being assigned.
var i2 = i.i2;
//~^ ERROR: Variable 'i' is used before being assigned.
