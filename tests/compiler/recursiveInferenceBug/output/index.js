// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveInferenceBug.ts`, Apache-2.0 License
function f(x) {
  var z = f(x);
  return x;
}
var zz = {
  g: () => {},
  get f() {
    return 'abc';
  }  
};