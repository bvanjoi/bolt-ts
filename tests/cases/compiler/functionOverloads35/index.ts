// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads35.ts`, Apache-2.0 License

function foo(bar:{a:number;}):number;
function foo(bar:{a:string;}):string;
function foo(bar:{a:any;}):any{ return bar }
var x = foo({a:1});
