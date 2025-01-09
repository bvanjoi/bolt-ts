// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/inOperator.ts`, Apache-2.0 License

var a=[];

for (var x in a) {}

if (3 in a) {}

var b = '' in 0;
//~^ ERROR: Type 'number' is not assignable to type 'object'.

var c: any;
var y: number;
if (y in c) { }
