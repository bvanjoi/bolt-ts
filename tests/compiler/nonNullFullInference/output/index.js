function testNonNullInference(numbers) {
  var last;
  for ( var n of numbers) {
    if (n % 2) {
      return n
    }
    
    last = n;
  }
  last;
  last;
}
function testNonNullInferenceWithArrays(numbers) {
  var result;
  var arr = [];
  for ( var n of numbers) {
    if (n % 2) {
      return [n]
    }
    
    arr.push(n);
    result = arr;
  }
  result;
  result;
}