
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/binopAssignmentShouldHaveType.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: lib=[es5]
'use strict';
var Test = {};
(function (Test) {

  class Bug {
    getName() {
      return 'name';
    }
    bug() {
      var name = null;
      if ((name = this.getName()).length > 0) {
        console.log(name);
      }
      
    }
  }
  Test.Bug = Bug;
  
})(Test);