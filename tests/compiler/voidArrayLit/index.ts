// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/voidArrayLit.ts`, Apache-2.0 License


var va = [(() => {})()]; // ok
(() => {})(); // ok
function foo(s:string) {}
foo((()=>{})()); // error
//~^ ERROR: Argument of type 'void' is not assignable to parameter of type 'string'.