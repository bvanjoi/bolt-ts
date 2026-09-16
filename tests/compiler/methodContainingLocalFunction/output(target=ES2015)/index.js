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