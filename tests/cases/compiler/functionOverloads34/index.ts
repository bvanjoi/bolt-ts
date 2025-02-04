// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads34.ts`, Apache-2.0 License

function foo(bar:{a:number;}):string;
function foo(bar:{a:boolean;}):number;
function foo(bar:{a:any;}):any{ return bar }
var x = foo();
//~^ ERROR: Expected 1 arguments, but got 0.
