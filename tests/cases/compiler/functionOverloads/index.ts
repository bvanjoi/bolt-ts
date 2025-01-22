// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads.ts`, Apache-2.0 License

function foo(): string;
function foo(bar: string): number;
function foo(bar?: string): any { return "" };
// var x = foo(5);

// var y: string = foo();
// var z: number = foo("''")