// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letShadowedByNameInNestedScope.ts`, Apache-2.0 License
var x;
function foo() {
  var x = 0;
  (function () {
    var _x = 1;
    console.log(x);
  })();
}