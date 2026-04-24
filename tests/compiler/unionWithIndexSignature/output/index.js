export function foo(arr) {
  var zz = arr[1];
}
export function isTypedArray(a) {
  return a instanceof Int32Array || a instanceof Uint8Array
}
export function flatten(arr) {
  if (isTypedArray(arr)) {
    arr[1];
  }
  
}