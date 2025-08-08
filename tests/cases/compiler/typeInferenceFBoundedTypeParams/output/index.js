function fold(values, result, fold) {
  for ( var value of values) {
    result = fold(result, value);
  }
  return result
}
function append(values, value) {
  values.push(value);
  return values
}
fold([1, 2, 3], [], (result, value) => (append(result, ['', ''])));