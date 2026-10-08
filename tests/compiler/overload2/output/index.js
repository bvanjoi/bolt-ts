// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overload2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
var A = {};
(function (A) {

})(A);
var B = {};
(// should be ok
function (B) {

})(B);
function foo(x) {}
class C {}
// should be ok
function foo1(x) {}