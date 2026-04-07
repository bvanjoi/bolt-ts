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