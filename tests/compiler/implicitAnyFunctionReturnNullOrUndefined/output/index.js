function nullWidenFunction() {
  return null;
}
function undefinedWidenFunction() {
  return undefined;
}
class C {
  nullWidenFuncOfC() {
    return null;
  }
  underfinedWidenFuncOfC() {
    return undefined;
  }
}
function foo1() {
  return null;
}
function bar1() {
  return undefined;
}
function fooBar() {
  return 1;
}
function fooFoo() {
  return 5;
}
nullWidenFunction();
undefinedWidenFunction();