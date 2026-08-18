// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicatesOptionalChaining1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var x = {
  y: {}  
};
function isNotNull(x) {
  return x !== null && x !== undefined;
}
function title(str) {
  return str.length > 0 ? 'Dear ' + str : 'Dear nobody';
}
isNotNull(x.y.z) ? title(x.y.z) : null;
if (isNotNull(x.y.z)) {
  var a = x.y;
}
