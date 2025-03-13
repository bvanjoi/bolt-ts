// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads15.ts`, Apache-2.0 License

function foo(foo:{a:string; b:number;}):string;
function foo(foo:{a:string; b:number;}):number;
function foo(foo:{a:string; b?:number;}):any { return "" }


foo({a: ''});
//~^ ERROR: No overload matches this call.

let a: number = foo({a: '', b: 0});
//~^ ERROR: Type 'string' is not assignable to type 'number'.