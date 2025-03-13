// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads2.ts`, Apache-2.0 License

function foo(bar: string): string;
function foo(bar: number): number;
function foo(bar: any): any { return bar };
var x = foo(true);
//~^ ERROR: No overload matches this call.