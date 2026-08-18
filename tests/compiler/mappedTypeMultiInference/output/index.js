// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeMultiInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var x = mergeStyleSets({}, {
  a: {
      flashy: true    
  }  
}, {
  b: {
      flashy: true    
  }  
});