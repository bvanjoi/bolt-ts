function simpleFunc() {
  return 'this is my simple func';
}
var simpleFuncVar = simpleFunc;
function anotherFuncNoReturn() {}
var anotherFuncNoReturnVar = anotherFuncNoReturn;
function withReturn() {
  return 'Hello';
}
var withReturnVar = withReturn;
function withParams(a) {
  return a;
}
var withparamsVar = withParams;
function withMultiParams(a, b, c) {
  return a;
}
var withMultiParamsVar = withMultiParams;
function withOptionalParams(a) {}
var withOptionalParamsVar = withOptionalParams;
function withInitializedParams(a, b0, b = 30, c = 'string value') {}
var withInitializedParamsVar = withInitializedParams;
function withOptionalInitializedParams(a, c = 'hello string') {}
var withOptionalInitializedParamsVar = withOptionalInitializedParams;
function withRestParams(a, ...myRestParameter) {
  return myRestParameter;
}
var withRestParamsVar = withRestParams;
function overload1(ns) {
  return ns.toString();
}
var withOverloadSignature = overload1;
function f(n) {}
var m2 = {};
(function (m2) {

  function foo(n) {}
  m2.foo = foo;
  
})(m2);
m2.foo(() => {
  var b = 30;
  return b;
});
var f2 = () => ('string');