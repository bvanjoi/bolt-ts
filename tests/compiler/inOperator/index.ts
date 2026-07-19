// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inOperator.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var a=[];

for (var x in a) {}

if (3 in a) {}

var b = '' in 0;
//~^ ERROR: The right value of the `in` operator must be an 'object', but got 'number'.

var c: any;
var y: number;
if (y in c) { }
