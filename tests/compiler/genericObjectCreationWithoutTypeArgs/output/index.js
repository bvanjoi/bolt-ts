// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericObjectCreationWithoutTypeArgs.ts`, Apache-2.0 License
class SS {}
var x1 = new SS();
var x2 = new SS();
// OK
var x3 = new SS();
var // OK 
x4 = new SS();
var x5// OK
// OK
 = new SS();