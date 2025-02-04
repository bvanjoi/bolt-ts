// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads33.ts`, Apache-2.0 License

function foo(bar:string):string;
function foo(bar:any):number;
function foo(bar:any):any{ return bar }
var x = foo(5);

var y: number = foo(5);
var z: string = foo('s');
