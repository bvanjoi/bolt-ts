// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadingOnConstants2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
   private x = 1;
}
class D extends C {}
class E { 
   private y = 1;
}
function foo(x: "hi", items: string[]): D;
function foo(x: "bye", items: string[]): E;
//~^ ERROR: This overload signature is not compatible with its implementation signature.
function foo(x: string, items: string[]): C {
   return null;
   //~^ ERROR: Type 'null' is not assignable to type 'C'.
}
var a: D = foo("hi", []); // D
var b: E = foo("bye", []); // E 
var c = foo("um", []); // error
//~^ ERROR: No overload matches this call.


//function bar(x: "hi", items: string[]): D;
function bar(x: "bye", items: string[]): E;
//~^ ERROR: This overload signature is not compatible with its implementation signature.
function bar(x: string, items: string[]): C;
function bar(x: string, items: string[]): C {
   return null;
//~^ ERROR: Type 'null' is not assignable to type 'C'.
}

var d: D = bar("hi", []); // D
var e: E = bar("bye", []); // E 
var f: C = bar("um", []); // C