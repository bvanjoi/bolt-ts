// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/objectLiteral1.ts`, Apache-2.0 License
var v30 = {
  a: 1,
  b: 2  
};
var v31 = {
  123: 123,
  [123.456]: 123.456  
};
var a0 = v31[123];
var a1 = v31[123.456];
var a2 = v31['123'];
var a3 = v31['123.456'];