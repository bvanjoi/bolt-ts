// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionMergedWithModule.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo(title) {
  var x = 10;
}

(function (foo) {

  var Bar = {};
  (function (Bar) {
  
    function f() {}
    Bar.f = f;
    
  })(Bar);
  foo.Bar = Bar;
  
})(foo);

(function (foo) {

  var Baz = {};
  (function (Baz) {
  
    function g() {
      Bar.f();
    }
    Baz.g = g;
    
  })(Baz);
  foo.Baz = Baz;
  
})(foo);