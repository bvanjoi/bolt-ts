// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/primitiveUnionDetection.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration
//@run-fail
var result = getInterfaceFromString({
  type: 'two'  
});