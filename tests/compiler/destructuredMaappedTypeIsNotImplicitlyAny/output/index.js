function foo(key, obj) {
  var {[key]: bar} = obj;
  bar;
  var lorem = obj[key];
}