var bar = {};
(function (bar_1) {

  function bar() {
    return this
  }
  bar_1.bar = bar;
  
})(bar);
var z = bar.bar();