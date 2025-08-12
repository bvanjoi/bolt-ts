// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads14.ts`, Apache-2.0 License

function foo():{a:number;}
function foo():{a:string;}
function foo():{a:any;} { return {a:1} }

let a: {a: string} = foo();
//~^ ERROR: Type '{ a: number; }' is not assignable to type '{ a: string; }'.