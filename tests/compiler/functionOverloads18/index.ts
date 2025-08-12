// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads18.ts`, Apache-2.0 License

function foo(bar:{a:number;});
//~^ ERROR: This overload signature is not compatible with its implementation signature.
function foo(bar:{a:string;}) { return {a:""} }
