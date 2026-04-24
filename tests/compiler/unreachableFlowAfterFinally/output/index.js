function f() {
  var x = 100;
  try {
    throw 'WAT'
  } catch (e) {}finally {
    return x
  }
}