// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multiCallOverloads.ts`, Apache-2.0 License
function load(f) {}
var f1 = function (z) {};
var f2 = function (z) {};
load(f1);
load(f2);
load(function () {});
load(function (z) {});