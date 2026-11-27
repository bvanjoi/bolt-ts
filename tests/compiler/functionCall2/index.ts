// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionCall2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo():number{return 1}; 
var x = foo();

{
  bar();
  //~^ ERROR: Cannot find name 'bar'.
  {
    function bar() {}
  }
  bar();
  //~^ ERROR: Cannot find name 'bar'.
}

{
  "use strict";
  far();
  //~^ ERROR: Cannot find name 'far'.
  {
    function far() {}
  }
  far(); 
  //~^ ERROR: Cannot find name 'far'.
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