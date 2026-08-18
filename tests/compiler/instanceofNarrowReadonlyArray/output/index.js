// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/instanceofNarrowReadonlyArray.ts`, Apache-2.0 License
function narrow(x) {
  if (x instanceof Array) {
    return x;
  } else {
    return [x];
  }
  
}
var r1 = narrow([1, 2, 3]);
var r2 = narrow(42);