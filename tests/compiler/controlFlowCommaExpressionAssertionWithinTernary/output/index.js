function foo2(param) {
  var val = param !== undefined;
  return val ? (assert(param !== undefined) , param) : null;
}