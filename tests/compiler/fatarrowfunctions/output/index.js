function foo(x) {
  return x()
}
foo((x, y, z) => (x + y + z));
foo((x, y, z) => (x + y + z));
foo((x, y, z) => (x + y + z));
foo((x, y, z) => (x + y + z));
foo((x, y, z) => (x + y + z));
foo(() => (0));
foo((x, y, z) => (x + y + z));
foo((x, y, z) => (x + y + z));
foo((x, y, z) => (x + y + z));
foo((x, y, z) => (x + y + z));
foo((x, y, z) => (x + y + z));
foo(() => (0));
foo(((x) => (x)));
foo((x) => (x * x));
var y = (x) => (x * x);
var z = (x) => (x * x);
var w = () => (3);
function ternaryTest(isWhile) {
  var f = isWhile ? function (n) {
    return n > 0
  } : function (n) {
    return n === 0
  };
}
var messenger = {
  message: 'Hello World',
  start: function () {
    setTimeout(() => {
      this.message.toString();
    }, 3000);
  }  
};