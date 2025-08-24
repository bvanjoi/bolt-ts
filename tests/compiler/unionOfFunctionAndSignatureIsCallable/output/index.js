function f1(c1, c2, callable) {
  var a = c1();
  var b = c2();
  var c = callable();
}
function f2(fetcherParams) {
  var data = typeof fetcherParams === 'function' ? fetcherParams() : fetcherParams;
}