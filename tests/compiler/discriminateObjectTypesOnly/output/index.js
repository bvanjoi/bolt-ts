// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/discriminateObjectTypesOnly.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
var k = {
  toFixed: null  
};// OK, satisfies object

var q = {
  toFixed: null  
};
var h = {
  toString: null  
};// OK, satisfies object

var l = {
  toString: undefined  
};