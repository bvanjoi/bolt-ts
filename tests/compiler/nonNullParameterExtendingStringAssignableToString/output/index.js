function fn(one, two) {
  var three = Boolean() ? one : two;
  foo(one);
  foo(two);
  foo(three);
}