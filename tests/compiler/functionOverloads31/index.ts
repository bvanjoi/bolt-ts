// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads31.ts`, Apache-2.0 License

function foo(bar:string):string;
function foo(bar:number):number;
function foo(bar:any):any{ return bar }
var x = foo(5);

var y: number = foo(5);
var z: string = foo('s');