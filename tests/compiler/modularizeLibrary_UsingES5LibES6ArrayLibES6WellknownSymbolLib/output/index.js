function f(x, y, z) {
  return Array.from(arguments);
}
f(1, 2, 3);
var a = ['c', 'd'];
a[Symbol.isConcatSpreadable] = false;