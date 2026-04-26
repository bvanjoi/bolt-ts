// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ambiguousOverload.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function foof(bar: string, y): number;
function foof(bar: string, x): string;
function foof(bar: any): any { return bar };
var x: number = foof("s", null);
var y: string = foof("s", null);
//~^ ERROR: Type 'number' is not assignable to type 'string'.

function foof2(bar: string, x): string;
function foof2(bar: string, y): number;
function foof2(bar: any): any { return bar };
var x2: string = foof2("s", null);
var y2: number = foof2("s", null);
//~^ ERROR: Type 'string' is not assignable to type 'number'.
