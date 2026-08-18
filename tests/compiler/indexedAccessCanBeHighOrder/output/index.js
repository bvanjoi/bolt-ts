// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/indexedAccessCanBeHighOrder.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function impl(a, b) {
  var item = get(a, b);
  return find(item);
}
var o = {
  x: 42  
};
var r = impl(o, 'x');
r[0][r[1]] = o[r[1]];