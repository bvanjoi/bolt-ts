// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/restParamsWithNonRestParams.ts`, Apache-2.0 License

function foo(...b:number[]){}
foo(); // ok
function foo2(a:string, ...b:number[]){}
foo2(); // should be an error
//~^ ERROR: Expected at least 1 arguments, but got 0.
function foo3(a?:string, ...b:number[]){}
foo3(); // error but shouldn't be