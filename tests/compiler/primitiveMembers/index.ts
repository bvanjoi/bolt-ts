// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/primitiveMembers.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var x = 5;
var r = /yo/;
r.source;

x.toBAZ();
//~^ ERROR: Property 'toBAZ' does not exist on type 'number'.
x.toString();

var n = 0;
declare var N: Number;

n = N;  // should not work, as 'number' has a different brand
//~^ ERROR: Type 'Number' is not assignable to type 'number'.
N = n; // should work

var o: Object = {}
var f: Function = (x: string) => x.length;
var r2: RegExp = /./g;
var n2: Number = 34;
var s: String = "yo";
var b: Boolean = true;

var n3 = 5 || {};
//~^ ERROR: This kind of expression is always truthy.

class baz { public bar(): void { }; }
class foo extends baz { public bar(){ return undefined}; }


