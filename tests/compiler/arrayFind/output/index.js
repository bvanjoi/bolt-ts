// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arrayFind.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function isNumber(x) {
  return typeof x === 'number';
}
var arrayOfStringsNumbersAndBooleans = ['string', false, 0, 'strung', 1, true];
var foundNumber = arrayOfStringsNumbersAndBooleans.find(isNumber);
var readonlyArrayOfStringsNumbersAndBooleans = arrayOfStringsNumbersAndBooleans;
var readonlyFoundNumber = readonlyArrayOfStringsNumbersAndBooleans.find(isNumber);