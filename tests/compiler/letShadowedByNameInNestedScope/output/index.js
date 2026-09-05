var x;
function foo() {
  var x = 0;
  (function () {
    var _x = 1;
    console.log(x);
  })();
}