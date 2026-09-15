var s = Symbol();
var t = {};
var p = new Proxy(t, {});
Reflect.ownKeys({});
function* idGen() {
  var i = 10;
  while (i < 20) {
    yield i + 2;
  }
}