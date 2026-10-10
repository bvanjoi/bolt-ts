// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/voidFunctionAssignmentCompat.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var fa = function () {
  return 3;
};
fa = function () {}// should not work
;
var fv = function () {};
fv = function () {
  return 0;
}// should work
;
function execAny(callback) {
  return callback(0);
}
execAny(function () {})// should work
;
function execVoid(callback) {
  callback(0);
}
execVoid(function () {
  return 0;
});// should work

var fra = function () {
  return function () {};
}// should work
;
var frv = function () {
  return function () {
    return 0;
  };// should work
  
};
var fra3 = (function () {
  return function (v) {
    return v;
  };
})()// should work
;
var frv3 = (function () {
  return function () {
    return 0;
  };
})();