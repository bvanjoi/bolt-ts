// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads16.ts`, Apache-2.0 License

function foo(foo:{a:string;}):string;
function foo(foo:{a:string;}):number;
function foo(foo:{a:string; b?:number;}):any { return "" }

let a: number = foo({a: ''});
//~^ ERROR: Type 'string' is not assignable to type 'number'.