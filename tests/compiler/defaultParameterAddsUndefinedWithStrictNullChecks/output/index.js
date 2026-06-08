function f(addUndefined1 = 'J', addUndefined2) {
  return addUndefined1.length + (addUndefined2 || 0)
}
function g(addUndefined = 'J', addDefined) {
  return addUndefined.length + addDefined
}
var total = f() + f('a', 1) + f('b') + f(undefined, 2);
total = g('c', 3) + g(undefined, 4);
function foo1(x = 'string', b) {
  x.length;
}
function foo2(x = 'string', b) {
  x.length;
}
function foo3(x = 'string', b) {
  x.length;
  x = undefined;
}
function foo4(x = undefined, b) {
  x;
  x = undefined;
}
function allowsNull(val = '') {
  val = null;
  val = 'string and null are both ok';
}
allowsNull(null);
foo1(undefined, 1);
foo2(undefined, 1);
foo3(undefined, 1);
foo4(undefined, 1);
function removeUndefinedButNotFalse(x = true) {
  if (x === false) {
    return x
  }
  
}

function removeNothing(y = cond ? true : undefined) {
  if (y !== undefined) {
    if (y === false) {
      return y
    }
    
  }
  
  return true
}