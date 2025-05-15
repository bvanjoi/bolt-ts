// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/conditionalExpressions2.ts`, Apache-2.0 License
var a = false ? 1 : null;
var b = false ? undefined : 0;
var c = false ? 1 : 0;
var d = false ? false : true;
var e = false ? "foo" : "bar";
var f = false ? null : undefined;
var g = true ? {g: 5} : null;
var h = [{h: 5}, null];
function i() {
  if (true) {
    return {x: 5}
  } else {
    return null
  }
  
}