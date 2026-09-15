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

  var aa = {};
  __test2__.aa = aa
  
  ;
  
  var __val__aa = aa;
  __test2__.__val__aa = __val__aa
  
})(__test2__);
__test2__.__val__aa = __test1__.__val__obj4;