// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileRestParametersOfFunctionAndFunctionType.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
function f1(...args) {}
function f2(x) {}
function f3(x) {}
function f4() {}
function f5() {}
var f6 = () => ([10]);