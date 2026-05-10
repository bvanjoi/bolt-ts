(async () => {
  function foo(p) {
    return []
  }
  function bar(p) {
    return []
  }
  var a1 = [];
  while (true) {
    var a2 = foo(a1);
    a1 = await bar(a2);
  }
});