function fail(message) {
  throw new Error(message)
}
function withFew(values, haveFew, haveNone) {
  return values.length > 0 ? haveFew(values) : haveNone('No values.')
}
function id(value) {
  return value
}
var result = withFew([1, 2, 3], id, fail);