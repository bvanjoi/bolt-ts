// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonNullFullInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function testNonNullInference(numbers) {
  var last;
  for ( var n of numbers) {
    if (n % 2) {
      return n;
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
      return [n];
    }
    
    arr.push(n);
    result = arr;
  }
  result;
  result;
}