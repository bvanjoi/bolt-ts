// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unionWithIndexSignature.ts`, Apache-2.0 License
//@compiler-options: target=es2015
export function foo(arr) {
  var zz = arr[1];
}
export function isTypedArray(a) {
  return a instanceof Int32Array || a instanceof Uint8Array;
}
export function flatten(arr) {
  if (isTypedArray(arr)) {
    arr[1];
  }
  
}