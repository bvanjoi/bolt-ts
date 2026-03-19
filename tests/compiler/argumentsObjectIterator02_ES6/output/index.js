function doubleAndReturnAsArray(x, y, z) {
  var blah = arguments[Symbol.iterator];
  var result = [];
  for ( var arg of blah()) {
    result.push(arg + arg);
  }
  return result
}