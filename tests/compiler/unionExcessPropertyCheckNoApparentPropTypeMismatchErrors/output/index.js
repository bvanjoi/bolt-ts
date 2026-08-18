// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unionExcessPropertyCheckNoApparentPropTypeMismatchErrors.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
var count = 0;
forEach({
  toString: 123  
}, () => (count++));