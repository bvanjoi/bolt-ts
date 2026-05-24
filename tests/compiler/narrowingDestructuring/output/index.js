function func(value) {
  if (value.kind === 'a') {
    value.a;
    var {a} = value;
  } else {
    value.b;
    var {b} = value;
  }
  
}
function func2(value) {
  if (value.kind === 'f') {
    var {f: f1} = value;
    var {f: {a, spread}} = value;
    value.f;
  } else {
    var {g: {c, spread}} = value;
    value.g;
  }
  
}
function func3(t) {
  if (t.kind === 'a') {
    var {kind, r1} = t;
    var r2 = (({kind, rest}) => (rest))(t);
  }
  
}
function farr(x) {
  var [head, ...tail] = x;
  if (typeof x[0] === 'number') {
    var [head, ...tail] = x;
  }
  
}