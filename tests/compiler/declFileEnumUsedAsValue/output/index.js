// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileEnumUsedAsValue.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
var e = {};
(function (e) {

  e[e['a'] = 0] = 'a'
  e[e['b'] = 0] = 'b'
  e[e['c'] = 0] = 'c'
})(e);
var x = e;