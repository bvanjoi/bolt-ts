// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads28.ts`, Apache-2.0 License

function foo():string;
function foo(bar:string):number;
function foo(bar?:any):any{ return '' }
var t:any; var x = foo(t);
