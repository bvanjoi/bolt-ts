// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericMethodOverspecialization.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var names = ['list', 'table1', 'table2', 'table3', 'summary'];

var a = document.getElementById('list');
var elements = names.map(function (name) {
  return document.getElementById(name);
});
var xxx = elements.filter(function (e) {
  return !e.isDisabled;
});
var widths = elements.map(function (e) {
  return e.clientWidth;
});