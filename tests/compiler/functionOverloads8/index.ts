// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads8.ts`, Apache-2.0 License

function foo();
//~^ ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.
function foo(foo:string);
//~^ ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.
function foo(foo?:any){ return '' }

let a = foo();
let b = foo('1')
