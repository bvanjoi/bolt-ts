function func1() {
  var {firstKey} = func({
      keys: ['aa', 'bb']    
  });
  var a = firstKey;
  var {keys} = func({
      keys: ['aa', 'bb']    
  });
  var b = keys;
}
function func2() {
  var {keys, firstKey} = func({
      keys: ['aa', 'bb']    
  });
  var a = firstKey;
  var b = keys;
}
function func3() {
  var x = func({
      keys: ['aa', 'bb']    
  });
  var a = x.firstKey;
  var b = x.keys;
}