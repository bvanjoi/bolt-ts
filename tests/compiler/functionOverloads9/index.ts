// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads9.ts`, Apache-2.0 License

function foo(foo:string);
function foo(foo?:string){ return '' };
var x = foo('foo');
