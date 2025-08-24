// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/assignmentCompat1.ts`, Apache-2.0 License

var x = { one: 1 };
var y: { [index: string]: any };
var z: { [index: number]: any };
x = y;  // Error
//~^ ERROR: Property 'one' is missing.
y = x;  // Ok because index signature type is any
x = z;  // Error
//~^ ERROR: Property 'one' is missing.
z = x;  // Ok because index signature type is any
y = "foo"; // Error
//~^ ERROR: Type 'string' is not assignable to type '{ [index: string]: any }'.
z = "foo"; // OK, string has numeric indexer
z = false; // Error
//~^ ERROR: Type 'boolean' is not assignable to type '{ [index: number]: any }'.

