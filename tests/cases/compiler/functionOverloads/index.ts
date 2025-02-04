// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads.ts`, Apache-2.0 License

function foo(): string;
function foo(bar: string): number;
function foo(bar?: string): any { return "" };
var x = foo(5);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.

var y: string = foo();
var z: number = foo("''")