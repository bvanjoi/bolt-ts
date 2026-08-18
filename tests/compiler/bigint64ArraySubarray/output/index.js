// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/bigint64ArraySubarray.ts`, Apache-2.0 License
function bigInt64ArraySubarray() {
  var arr = new BigInt64Array(10);
  arr.subarray();
  arr.subarray(0);
  arr.subarray(0, 10);
}
var a = 1n;
function f(_) {}
f(a);
f(1n);