var __test1__ = {};
(function (__test1__) {

  ;
  
  var obj4 = {
      one: 1    
  };
  
  ;
  
  var __val__obj4 = obj4;
  __test1__.__val__obj4 = __val__obj4
  
})(__test1__);
var __test2__ = {};
(function (__test2__) {

  class classWithPublicAndOptional {
    constructor(one, two) {
      this.one = one
      
      this.two = two}
  }
  __test2__.classWithPublicAndOptional = classWithPublicAndOptional;
  
  var x4 = new classWithPublicAndOptional(1);
  
  ;
  
  var __val__x4 = x4;
  __test2__.__val__x4 = __val__x4
  
})(__test2__);
__test2__.__val__x4 = __test1__.__val__obj4;