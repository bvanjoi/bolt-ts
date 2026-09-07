function baz(x) {
  return [[x, x]];
}
function foo(set) {
  for ( var [value, i] of baz(set.values)) {
    var bar = [];
    (() => (bar));
    set.values.push(...[]);
  }
}
;