
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