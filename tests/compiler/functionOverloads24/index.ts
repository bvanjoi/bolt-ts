// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionOverloads24.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function foo(bar:number):(b:string)=>void;
function foo(bar:string):(a:number)=>void;
function foo(bar:any):(a)=>void { return function(){} }

let x = foo(5);
let x0: (b: string) => void = x;

let y = foo('');
let y0: (a: number) => void = y;
