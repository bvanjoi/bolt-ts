var x = {
  y: {}  
};
function isNotNull(x) {
  return x !== null && x !== undefined
}
function title(str) {
  return str.length > 0 ? 'Dear ' + str : 'Dear nobody'
}
isNotNull(x.y.z) ? title(x.y.z) : null;
if (isNotNull(x.y.z)) {
  var a = x.y;
}
