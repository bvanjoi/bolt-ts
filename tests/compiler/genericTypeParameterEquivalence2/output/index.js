// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericTypeParameterEquivalence2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
// compose :: (b->c) -> (a->b) -> (a->c)
function compose(f, g) {
  return function (a) {
    return f(g.apply(null, a));
  };
}
// forEach :: [a] -> (a -> ()) -> ()
function forEach(list, f) {
  for ( var i = 0; i < list.length; ++i) {
    f(list[i], i);
  }
}
// filter :: (a->bool) -> [a] -> [a]
function filter(f, ar) {
  var ret = [];
  forEach(ar, (el) => {
    if (f(el)) {
      ret.push(el);
    }
    
  });
  return ret;
}
// length :: [a] -> Num
function length2(ar) {
  return ar.length;
}
// curry1 :: ((a,b)->c) -> (a->(b->c))
function curry1(f) {
  return function (ay) {
    return function (by) {
      return f(ay, by);
    };
  };
}
var cfilter = curry1(filter);
// compose :: (b->c) -> (a->b) -> (a->c)
// length :: [a] -> Num
// cfilter :: {} -> {} -> [{}]
// pred :: a -> Bool 
// cfilter(pred) :: {} -> [{}]
// length2 :: [a] -> Num
// countWhere :: (a -> Bool) -> [a] -> Num
function countWhere_1(pred) {
  return compose(length2, cfilter(pred));
}
function countWhere_2(pred) {
  var where = cfilter(pred);
  return compose(length2, where);
}