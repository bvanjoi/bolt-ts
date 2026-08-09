// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assigningFromObjectToAnythingElse.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var x: Object;
var y: RegExp;
y = x;
//~^ ERROR: Type 'Object' is missing the following properties from type 'RegExp': exec, test, and 13 more.

var a: String = Object.create<Object>("");
//~^ ERROR: Expected 0 type arguments, but got 1.
var c: String = Object.create<Number>(1);
//~^ ERROR: Expected 0 type arguments, but got 1.

var w: Error = new Object();
//~^ ERROR: Property 'message' is missing.
//~| ERROR: Property 'name' is missing.
