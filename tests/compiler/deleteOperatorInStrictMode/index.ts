// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/deleteOperatorInStrictMode.ts`, Apache-2.0 License

"use strict"
var a;
delete a;
//~^ ERROR: 'delete' cannot be called on an identifier in strict mode.
//~| ERROR: The operand of a 'delete' operator must be a property reference.