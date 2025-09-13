function foo():number{return 1}; 
var x = foo();

{
  bar();
  {
    function bar() {}
  }
  bar();
}

{
  "use strict";
  far();
  {
    function far() {}
  }
  far(); 
}

function f() {
  "use strict";
  ttt();
  //~^ ERROR: Cannot find name 'ttt'.
  {
    function ttt() {}
  }
  ttt();
  //~^ ERROR: Cannot find name 'ttt'.
}