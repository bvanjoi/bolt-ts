// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferTypeArgumentsInSignatureWithRestParameters.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function f(array, ...args) {}
function g(array, ...args) {}
function h(nonarray, ...args) {}
function i(array, opt) {}
var a = [1, 2, 3, 4, 5];
f(a);
g(a);
h(a);
i(a);