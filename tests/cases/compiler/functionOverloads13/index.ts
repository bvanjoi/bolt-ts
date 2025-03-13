// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads13.ts`, Apache-2.0 License

function foo(bar:number):string;
function foo(bar:number):number;
function foo(bar?:number):any { return "" }

let a: number = foo(1);
//~^ ERROR: Type 'string' is not assignable to type 'number'.
let b: string = foo(1);
