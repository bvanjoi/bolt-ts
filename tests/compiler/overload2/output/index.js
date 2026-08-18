// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overload2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var A = {};
(function (A) {

})(A);
var B = {};
(function (B) {

})(B);
function foo(x) {}
class C {}
function foo1(x) {}