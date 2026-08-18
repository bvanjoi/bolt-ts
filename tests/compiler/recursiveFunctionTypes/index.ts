// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveFunctionTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015


function fn(): typeof fn { return 1; }
//~^ ERROR: Type 'number' is not assignable to type '() => any'.

var x: number = fn; // error
//~^ ERROR: Type '() => any' is not assignable to type 'number'.
var y: () => number = fn; // ok
//~^ ERROR: Type '() => any' is not assignable to type '() => number'.

var f: () => typeof g;
var g: () => typeof f;

function f1(d: typeof f1) { }

function f2(): typeof g2 { } 
//~^ ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
function g2(): typeof f2 { } 
//~^ ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.

interface I<T> { }
function f3(): I<typeof f3> { return f3; }

var a: number = f3; // error
//~^ ERROR: Type '() => I<any>' is not assignable to type 'number'.

class C {
     static g(t: typeof C.g){ }
}
C.g(3); // error
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type '(t: any) => void'.

var f4: () => typeof f4;
f4 = 3; // error
//~^ ERROR: Type 'number' is not assignable to type '() => any'.

function f5() { return f5; }

function f6(): typeof f6;
function f6(a: typeof f6): () => number;
//~^ ERROR: This overload signature is not compatible with its implementation signature.
function f6(a?: any) { return f6; }

f6("", 3); // error (arity mismatch)
//~^ ERROR: Expected 0-1 arguments, but got 2.
f6(""); // ok (function takes an any param)
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type '() => any'.
f6(); // ok

declare function f7(): typeof f7;
declare function f7(a: typeof f7): () => number;
declare function f7(a: number): number;
declare function f7(a?: typeof f7): typeof f7;

f7("", 3); // error (arity mismatch)
//~^ ERROR: Expected 0-1 arguments, but got 2.
f7(""); // ok (function takes an any param)
//~^ ERROR: No overload matches this call.
f7(); // ok