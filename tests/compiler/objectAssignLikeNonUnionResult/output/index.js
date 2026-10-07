// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectAssignLikeNonUnionResult.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
var defaultValue = {
  field: 1  
};
// Displayed type: Interface & { field: number }
// Underlying type: Something else...
var data1 = assign(defaultValue, Date.now() > 3 ? {
  field: 2  
} : {});// ???
// 'X' | [unknown] -- why?
