// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/specializedSignatureAsCallbackParameter1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function x3(a: number, cb: (x: number) => number);
function x3(a: string, cb: (x: number) => number);
function x3(a: any, cb: (x: number) => number) {
   cb(a);
}
// both are errors
x3(1, (x: string) => 1); 
//~^ ERROR: No overload matches this call.
x3(1, (x: 'hm') => 1); 
//~^ ERROR: No overload matches this call.
