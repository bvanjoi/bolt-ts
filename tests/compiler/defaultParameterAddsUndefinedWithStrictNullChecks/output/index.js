// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defaultParameterAddsUndefinedWithStrictNullChecks.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@compiler-options: declaration
function f(addUndefined1 = 'J', addUndefined2) {
  return addUndefined1.length + (addUndefined2 || 0);
}
function g(addUndefined = 'J', addDefined) {
  return addUndefined.length + addDefined;
}
var total = f() + f('a', 1) + f('b') + f(undefined, 2);
total = g('c', 3) + g(undefined, 4);
function foo1(x = 'string', b) {
  x.length;
}
function foo2(x = 'string', b) {
  x.length;
// ok, should be string
}
function foo3(x = 'string', b) {
  x.length;
  // ok, should be string
  x = undefined;
}
function foo4(x = undefined, b) {
  x;
  // should be string | undefined
  x = undefined;
}
function allowsNull(val = '') {
  val = null;
  val = 'string and null are both ok';
}
allowsNull(null);
// still allows passing null
// .d.ts should have `string | undefined` for foo1, foo2, foo3 and foo4
foo1(undefined, 1);
foo2(undefined, 1);
foo3(undefined, 1);
foo4(undefined, 1);
function removeUndefinedButNotFalse(x = true) {
  if (x === false) {
    return x;
  }
  
}

function removeNothing(y = cond ? true : undefined) {
  if (y !== undefined) {
    if (y === false) {
      return y;
    }
    
  }
  
  return true;
}