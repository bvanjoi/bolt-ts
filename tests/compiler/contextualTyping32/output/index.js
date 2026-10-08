// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/contextualTyping32.ts`, Apache-2.0 License
function foo(param) {}
;
foo([function () {
  return 1;
}, function () {
  return 4;
}]);