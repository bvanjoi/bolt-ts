// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads29.ts`, Apache-2.0 License

function foo(bar:string):string;
function foo(bar:number):number;
function foo(bar:any):any{ return bar }
var x = foo();
//~^ Expected 1 arguments, but got 0.
var y = foo(1, 2);
//~^ Expected 1 arguments, but got 2.
