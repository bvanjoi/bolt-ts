// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/emptyIndexer.ts`, Apache-2.0 License

interface I1 {
	m(): number;
}

interface I2 {
	[s:string]: I1;
}


var x: I2;

var n = x[''].m(); // should not crash compiler
var n0: string = x[''].m();
//~^ ERROR: Type 'number' is not assignable to type 'string'.