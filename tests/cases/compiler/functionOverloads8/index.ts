// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads8.ts`, Apache-2.0 License

function foo();
function foo(foo:string);
function foo(foo?:any){ return '' }

let a = foo();
let b = foo('1')