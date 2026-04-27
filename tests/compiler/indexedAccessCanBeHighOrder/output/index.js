function impl(a, b) {
  var item = get(a, b);
  return find(item)
}
var o = {
  x: 42  
};
var r = impl(o, 'x');
r[0][r[1]] = o[r[1]];