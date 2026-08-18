// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericTypeParameterEquivalence2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
function compose(f, g) {
  return function (a) {
    return f(g.apply(null, a));
  };
}
function forEach(list, f) {
  for ( var i = 0; i < list.length; ++i) {
    f(list[i], i);
  }
}
function filter(f, ar) {
  var ret = [];
  forEach(ar, (el) => {
    if (f(el)) {
      ret.push(el);
    }
    
  });
  return ret;
}
function length2(ar) {
  return ar.length;
}
function curry1(f) {
  return function (ay) {
    return function (by) {
      return f(ay, by);
    };
  };
}
var cfilter = curry1(filter);
function countWhere_1(pred) {
  return compose(length2, cfilter(pred));
}
function countWhere_2(pred) {
  var where = cfilter(pred);
  return compose(length2, where);
}