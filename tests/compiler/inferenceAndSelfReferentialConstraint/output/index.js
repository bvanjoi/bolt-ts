function test(arg) {
  return arg;
}
var res1 = test({
  foo: true,
  bar() {}  
});
var res2 = test({
  foo: true,
  bar: function () {}  
});
var res3 = test({
  foo: true,
  bar: () => {}  
});