// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/duplicatePropertiesInStrictMode.ts`, Apache-2.0 License

"use strict";
var x = {
  x: 1,
  x: 2
  //~^ ERROR: An object literal cannot have multiple properties with the same name.
}
