// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads32.ts`, Apache-2.0 License

function foo(bar:string):string;
function foo(bar:number):number;
function foo(bar:any):any{ return bar }
var baz:number; var x = foo(baz);
//~^ ERROR: Variable 'baz' is used before being assigned.
//~| ERROR: Variable 'baz' is used before being assigned.

var k: string = x;
//~^ ERROR: Type 'number' is not assignable to type 'string'.
