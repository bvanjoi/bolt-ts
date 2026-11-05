// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multiCallOverloads.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function load(f) {}
var f1 = function (z) {};
var f2 = function (z) {};
load(f1)// ok
;
load(f2)// ok
;
load(function () {})// this shouldn’t be an error
;
load(function (z) {});