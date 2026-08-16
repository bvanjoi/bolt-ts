// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/literals-negative.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// Type type of the null literal is the Null type.
// Null can be converted to anything except Void
var n = <number>(null);
//~^ ERROR: Conversion of type 'null' to type 'number' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
var s = <string>(null);
//~^ ERROR: Conversion of type 'null' to type 'string' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
var b = <boolean>(n);
//~^ ERROR: Conversion of type 'number' to type 'boolean' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.

function isVoid() : void { }

// Expected error: Values of type null and void cannot be compared
if(null === isVoid()) { }
