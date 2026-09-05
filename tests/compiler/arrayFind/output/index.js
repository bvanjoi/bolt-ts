function isNumber(x) {
  return typeof x === 'number';
}
var arrayOfStringsNumbersAndBooleans = ['string', false, 0, 'strung', 1, true];
var foundNumber = arrayOfStringsNumbersAndBooleans.find(isNumber);
var readonlyArrayOfStringsNumbersAndBooleans = arrayOfStringsNumbersAndBooleans;
var readonlyFoundNumber = readonlyArrayOfStringsNumbersAndBooleans.find(isNumber);