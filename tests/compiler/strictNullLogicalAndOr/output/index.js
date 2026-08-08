var sinOrCos = Math.random() < 0.5;
var choice = sinOrCos && Math.sin || Math.cos;
choice(Math.PI);
function sq(n) {
  var r = n !== undefined && n * n || 0;
  return r;
}
sq(3);