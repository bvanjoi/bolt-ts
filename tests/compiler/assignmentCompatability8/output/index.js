// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatability8.ts`, Apache-2.0 License
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

  class classWithPublic {
    constructor(one) {
      this.one = one}
  }
  __test2__.classWithPublic = classWithPublic;
  
  var x1 = new classWithPublic(1);
  
  ;
  
  var __val__x1 = x1;
  __test2__.__val__x1 = __val__x1
  
})(__test2__);
__test2__.__val__x1 = __test1__.__val__obj4;