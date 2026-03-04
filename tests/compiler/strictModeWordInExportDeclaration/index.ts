// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/strictModeWordInExportDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=ES6

"use strict"
var x = 1;
export { x as foo }
export { x as implements }
export { x as while }