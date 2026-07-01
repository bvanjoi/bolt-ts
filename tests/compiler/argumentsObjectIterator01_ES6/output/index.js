function doubleAndReturnAsArray(x, y, z) {
  var result = [];
  for ( var arg of arguments) {
    result.push(arg + arg);
  }
  return result;
}