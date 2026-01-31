function recursive() {
  var x = (subkey) => (recursive());
  return x
}
var result = recursive()(1);