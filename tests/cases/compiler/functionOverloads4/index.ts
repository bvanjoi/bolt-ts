// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads4.ts`, Apache-2.0 License

function foo():number;
//~^ ERROR: This overload signature is not compatible with its implementation signature.
function foo():string { return "a" }