// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads19.ts`, Apache-2.0 License

function foo(bar:{b:string;});
//~^ ERROR: This overload signature is not compatible with its implementation signature.
function foo(bar:{a:string;});
function foo(bar:{a:any;}) { return {a:""} }
