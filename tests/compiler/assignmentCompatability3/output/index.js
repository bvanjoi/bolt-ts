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

  var obj = {
      one: 1    
  };
  __test2__.obj = obj
  
  var __val__obj = obj;
  __test2__.__val__obj = __val__obj
  
})(__test2__);
__test2__.__val__obj = __test1__.__val__obj4;