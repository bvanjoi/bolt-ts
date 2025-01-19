var x = { one: 1 };
var y: { [index: string]: any };
var z: { [index: number]: any };
x = y;  // Error
//~^ ERROR: Property 'one' is missing.
y = x;  // Ok because index signature type is any
x = z;  // Error
//~^ ERROR: Property 'one' is missing.
z = x;  // Ok because index signature type is any
// TODO:
// y = "foo"; // Error
// z = "foo"; // OK, string has numeric indexer
// z = false; // Error

