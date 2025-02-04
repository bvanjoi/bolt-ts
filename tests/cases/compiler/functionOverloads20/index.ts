// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads20.ts`, Apache-2.0 License

function foo(bar:{a:number;}): number;
//~^ ERROR: This overload signature is not compatible with its implementation signature.
function foo(bar:{a:string;}): string;
function foo(bar:{a:any;}): string {return ""}

let a: number = foo({a: 1});
let b: string = foo({a: ""});