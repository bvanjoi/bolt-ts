// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferSecondaryParameter.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var b = {
  m: function (test, fn) {}  
};
b.m('test', function (bug) {
  var a = bug;
});