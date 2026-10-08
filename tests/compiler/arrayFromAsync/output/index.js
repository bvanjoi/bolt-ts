// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arrayFromAsync.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@compiler-options: module=esnext
//@compiler-options: target=esnext
//@run-fail
export {  }
async function* asyncGen(n) {
  for ( var i = 0; i < n; i++) yield i * 2;
}
function* genPromises(n) {
  for ( var i = 0; i < n; i++) {
    yield Promise.resolve(i * 2);
  }
}
var arrLike = {
  0: Promise.resolve(0),
  1: Promise.resolve(2),
  2: Promise.resolve(4),
  3: Promise.resolve(6),
  length: 4  
};
var arr = [];
for await ( var v of asyncGen(4)) {
  arr.push(v);
}
var sameArr1 = await Array.fromAsync(arrLike);
var sameArr2 = await Array.fromAsync([Promise.resolve(0), Promise.resolve(2), Promise.resolve(4), Promise.resolve(6)]);
var sameArr3 = await Array.fromAsync(genPromises(4));
var sameArr4 = await Array.fromAsync(asyncGen(4));
function Data(n) {}
Data.fromAsync = Array.fromAsync;
var sameArr5 = await Data.fromAsync(asyncGen(4));
var mapArr1 = await Array.fromAsync(asyncGen(4), (v) => (v ** 2));
var mapArr2 = await Array.fromAsync([0, 2, 4, 6], (v) => (Promise.resolve(v ** 2)));
var mapArr3 = await Array.fromAsync([0, 2, 4, 6], (v) => (v ** 2));
var err = new Error();
var badIterable = {
  [Symbol.iterator]() {
    throw err
  }  
};
var // This returns a promise that will reject with `err`.
badArray = await Array.fromAsync(badIterable);
var withIndexResult = await Array.fromAsync(['a', 'b'], (str, index) => (({
  index,
  str  
})));