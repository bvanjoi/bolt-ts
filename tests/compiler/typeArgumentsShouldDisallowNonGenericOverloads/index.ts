// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeArgumentsShouldDisallowNonGenericOverloads.ts`, Apache-2.0 License

function foo(a: string): string;
function foo<T>(a: T): number;
function foo(a: any): any {
    return "hi";
}

var x: number = foo<string>("hi"); // return type should be 'number'
var y: string = foo("hi"); // return type should be 'string'

var w: string = foo<string>("hi"); // should error
//~^ ERROR: Type 'number' is not assignable to type 'string'.
var z: number = foo("hi"); // should error
//~^ ERROR: Type 'string' is not assignable to type 'number'.
