// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x = <() => number>null;
//~^ ERROR: Conversion of type 'null' to type '() => number' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
var y = <{(): number; }>null;
//~^ ERROR: Conversion of type 'null' to type '() => number' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
var z = <{new(): number; }>null
//~^ ERROR: Conversion of type 'null' to type 'new () => number' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
var w = <{[x:number]: number; }>null
//~^ ERROR: Conversion of type 'null' to type '{ [x: number]: number }' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
function f() { return 3 };
function g(s: string) { true };
y=f;
y=g;
//~^ ERROR: Type '(s: string) => void' is not assignable to type '() => number'.
x=g;
//~^ ERROR: Type '(s: string) => void' is not assignable to type '() => number'.
w=g;
//~^ ERROR: Type '(s: string) => void' is not assignable to type '{ [x: number]: number }'.
z=g;
//~^ ERROR: Type '(s: string) => void' is not assignable to type 'new () => number'.
