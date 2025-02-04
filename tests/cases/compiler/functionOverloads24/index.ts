// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads24.ts`, Apache-2.0 License

function foo(bar:number):(b:string)=>void;
function foo(bar:string):(a:number)=>void;
function foo(bar:any):(a)=>void { return function(){} }

let x = foo(5);
let x0: (b: string) => void = x;

let y = foo('');
let y0: (a: number) => void = y;