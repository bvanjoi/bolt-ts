// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads30.ts`, Apache-2.0 License

function foo(bar:string):string;
function foo(bar:number):number;
function foo(bar:any):any{ return bar }
var x = foo('bar');
