// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/v.ts`, Apache-2.0 License
function a() {
  var global = 1;
}
function b() {
  class global {}
}

function foo(global) {}
var obj = {
  global: '123'  
};