// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/methodContainingLocalFunction.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
// The first case here (BugExhibition<T>) caused a crash. Try with different permutations of features.
class BugExhibition {
  exhibitBug() {
    function localFunction() {}
    var x;
    x = localFunction;
  }
}
class BugExhibition2 {
  static get exhibitBug() {
    function localFunction() {}
    var x;
    x = localFunction;
    return null;
  }
}
class BugExhibition3 {
  exhibitBug() {
    function localGenericFunction(u) {}
    var x;
    x = localGenericFunction;
  }
}
class C {
  exhibit() {
    var funcExpr = (u) => {};
    var x;
    x = funcExpr;
  }
}
var M = {};
(function (M) {

  function exhibitBug() {
    function localFunction() {}
    var x;
    x = localFunction;
  }
  M.exhibitBug = exhibitBug;
  
})(M);
var E = {};
(function (E) {

  E[E['A'] = (() => {
    function localFunction() {}
    var x;
    x = localFunction;
    return 0;
  })()] = 'A'
})(E);