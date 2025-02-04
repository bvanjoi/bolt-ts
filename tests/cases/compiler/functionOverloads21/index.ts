// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads21.ts`, Apache-2.0 License

function foo(bar:{a:number;}[]);
function foo(bar:{a:number; b:string;}[]);
function foo(bar:{a:any; b?:string;}[]) { return 0 }
