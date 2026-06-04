var a = {};
var x = 0;
({
  x  
} = a);
function f1(options) {
  var {color, width} = options || {};
  ({
      color,
    width    
  } = options || {});
  var x1 = (options || {}).color;
  var x2 = (options || {})['color'];
}
function f2(options) {
  var [str, num] = options || [];
  [str, num] = options || [];
  var x1 = (options || {})[0];
}
function f3(options) {
  var {color, width} = options || {};
  ({
      color,
    width    
  } = options || {});
  var x1 = (options || {}).color;
  var x2 = (options || {})['color'];
}
function f4(options) {
  var [str, num] = options || [];
  [str, num] = options || [];
  var x1 = (options || {})[0];
}