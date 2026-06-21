function isFoo1(object) {
  return 'foo' in object
}
function isFoo2(object) {
  return 'foo' in object
}

if (isFoo1(test)) {
  test.foo = 'hi';
}

if (isFoo2(test)) {
  test.foo = 'hi';
}
