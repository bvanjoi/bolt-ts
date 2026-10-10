// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/widenedTypes.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=es2015
//@compiler-options: declaration

null instanceof (() => { });
//~^ ERROR: The left-hand side of an 'instanceof' expression must be of type 'any', an object type or a type parameter.
({}) instanceof null; // Ok because null is a subtype of function

null in {};
//~^ ERROR: The value 'null' cannot be used here.
"" in null;
//~^ ERROR: The value 'null' cannot be used here.

for (var a in null) { }

var t = [3, (3, null)];
//~^ ERROR: Left side of comma operator is unused and has no side effects.
t[3] = "";
//~^ ERROR: Type 'string' is not assignable to type 'number'.
var x: typeof undefined = 3;
x = 3;

var y;
var u = [3, (y = null)];
u[3] = "";
//~^ ERROR: Type 'string' is not assignable to type 'number'.

var ob: { x: typeof undefined } = { x: "" };

// Highlights the difference between array literals and object literals
var arr: string[] = [3, null]; // not assignable because null is not widened. BCT is {}
//~^ ERROR: Type 'number' is not assignable to type 'string'.
var obj: { [x: string]: string; } = { x: 3, y: null }; // assignable because null is widened, and therefore BCT is any
//~^ ERROR: Type 'number' is not assignable to type 'string'.
