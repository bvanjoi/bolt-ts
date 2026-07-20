// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionOverloads9.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function foo(foo:string);
function foo(foo?:string){ return '' };
var x = foo('foo');
