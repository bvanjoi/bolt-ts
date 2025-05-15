// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInModuleFunction1.ts`, Apache-2.0 License
var bar = {};
(function (bar_1) {

  function bar() {
    return this
  }
  bar_1.bar = bar;
  
})(bar);
var z = bar.bar();