// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defaultArgsInFunctionExpressions.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var f = function (a = 3) { return a; }; // Type should be (a?: number) => number
var n: number = f(4);
n = f();
var s: string = f('');
//~^ ERROR: Type 'number' is not assignable to type 'string'.
//~| ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
s = f();
//~^ ERROR: Type 'number' is not assignable to type 'string'.

// Type check the default argument with the type annotation
var f2 = function (a: string = 3) { return a; }; // Should error, but be of type (a: string) => string;
//~^ ERROR: Type 'number' is not assignable to type 'string'.
s = f2('');
s = f2();
n = f2();
//~^ ERROR: Type 'string' is not assignable to type 'number'.

// Contextually type the default arg with the type annotation
var f3 = function (a: (s: string) => any = (s) => <number>s) { };
//~^ ERROR: Conversion of type 'string' to type 'number' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.

// Type check using the function's contextual type
var f4: (a: number) => void = function (a = "") { };
//~^ ERROR: Type 'string' is not assignable to type 'number'.

// Contextually type the default arg using the function's contextual type
var f5: (a: (s: string) => any) => void = function (a = s => <number>s) { };
//~^ ERROR: Conversion of type 'string' to type 'number' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.

// Instantiated module
namespace T { }
namespace U {
    export var x;
}

var f6 = (t = T) => { };
//~^ ERROR: Cannot find name 'T'.
var f7 = (t = U) => { return t; };

f7().x;