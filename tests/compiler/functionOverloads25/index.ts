// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads25.ts`, Apache-2.0 License

function foo():string;
function foo(bar:string):number;
function foo(bar?:any):any{ return '' };
var x = foo();
var y: string = x;