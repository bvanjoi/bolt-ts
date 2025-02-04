// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads22.ts`, Apache-2.0 License

function foo(bar:number):{a:number;}[];
function foo(bar:string):{a:number; b:string;}[];
function foo(bar:any):{a:any;b?:any;}[] { return [{a:""}] }

let a: {a: number;}[] = foo(5);
let b: {a: number; b: string;}[] = foo('');