

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