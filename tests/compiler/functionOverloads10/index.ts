// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads10.ts`, Apache-2.0 License

function foo(foo:string, bar:number);
function foo(foo:string);
function foo(foo:any){ }

foo('', 1)
foo('')