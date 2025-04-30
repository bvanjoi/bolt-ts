var obj1;
var obj2 = {x: ""};
obj1 = {};
obj1 = obj2;
function f(x) {}
f({});
f(obj1);
f(obj2);