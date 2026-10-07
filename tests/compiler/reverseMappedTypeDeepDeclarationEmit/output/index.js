// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reverseMappedTypeDeepDeclarationEmit.ts`, Apache-2.0 License
//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: declaration
//@run-fail


//native validators
var test = {
  Test: {
      Test1: {
          Test2: SimpleStringValidator      
    }    
  }  
};
var validatorFunc = ObjValidator(test);
var outputExample = validatorFunc({
  Test: {
      Test1: {
          Test2: 'hi'      
    }    
  }  
});