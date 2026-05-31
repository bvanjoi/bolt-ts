// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionCall15.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo(a?:string, b?:number, ...b:number[]){}
//~^ ERROR: Duplicate identifier 'b'.