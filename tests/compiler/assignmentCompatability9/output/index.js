// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatability9.ts`, Apache-2.0 License
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

  class classWithOptional {
    constructor(one) {
      this.one = one}
  }
  __test2__.classWithOptional = classWithOptional;
  
  var x3 = new classWithOptional();
  
  ;
  
  var __val__x3 = x3;
  __test2__.__val__x3 = __val__x3
  
})(__test2__);
__test2__.__val__x3 = __test1__.__val__obj4;