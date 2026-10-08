// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/substitutionTypeNoMergeOfAssignableType.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function makeEntityStore(config) {
  return {};
}
var myTest = makeEntityStore({
  test: {
      fields: {
          id: {}      
    }    
  }  
});
myTest.test;