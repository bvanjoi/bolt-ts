// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferRestArgumentsMappedTuple.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
var myPrimitiveTupleOld = extractPrimitivesOld({
  primitive: ''  
}, {
  primitive: 0  
});
var myPrimitiveTupleNew = extractPrimitivesNew({
  primitive: ''  
}, {
  primitive: 0  
});