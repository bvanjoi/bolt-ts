// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/excessPropertyCheckWithNestedArrayIntersection.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var repro = {
  dataType: {
      fields: [{
          key: 'bla',
      // should be OK: Not excess
      value: null      
    }]    
  }  
};