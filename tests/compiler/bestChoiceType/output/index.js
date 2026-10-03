(''.match(/ /) || []).map((s) => (s.toLowerCase()));
function f1() {
  var x = ''.match(/ /);
  var y = x || [];
  var z = y.map((s) => (s.toLowerCase()));
}
function f2() {
  var x = ''.match(/ /);
  var y = x ? x : [];
  var z = y.map((s) => (s.toLowerCase()));
}