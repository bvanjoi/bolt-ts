// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadResolutionTest1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function foo(bar:{a:number;}[]):string;
function foo(bar:{a:boolean;}[]):number;
function foo(bar:{a:any;}[]):any{ return bar };

var x1 = foo([{a:true}]); // works
var x11 = foo([{a:0}]); // works
var x111 = foo([{a:"s"}]); // error - does not match any signature
//~^ ERROR: No overload matches this call.
var x1111 = foo([{a:null}]); // works - ambiguous call is resolved to be the first in the overload set so this returns a string



function foo2(bar:{a:number;}):string;
function foo2(bar:{a:boolean;}):number;
function foo2(bar:{a:any;}):any{ return bar };

var x2 = foo2({a:0}); // works
var x3 = foo2({a:true}); // works
var x4 = foo2({a:"s"}); // error
//~^ ERROR: No overload matches this call.


function foo4(bar:{a:number;}):number;
function foo4(bar:{a:string;}):string;
function foo4(bar:{a:any;}):any{ return bar };
var x = foo4({a:true}); // error
//~^ ERROR: No overload matches this call.
