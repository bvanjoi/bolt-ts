// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doubleUnderscoreMappedTypes.ts`, Apache-2.0 License
//@compiler-options: target=es2015
// As expected, I can make an object satisfying this interface
var ok = {
  property1: '',
  __property2: ''  
};// As expected, "__property2" is indeed a key of the type

var k = '__property2';// ok
// This should be valid

// And should work with partial
var partial = {
  property1: '',
  __property2: ''  
};