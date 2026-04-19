var Bugs = {};
(function (Bugs) {

  class A {}
  
  function bug2(message, ...args) {
    var result = message.replace(/\{(\d+)\}/g, function (match, ...rest) {
      var index = rest[0];
      return typeof args[index] !== 'undefined' ? args[index] : match
    });
    return result
  }
  
})(Bugs);
function bug3(f) {
  return f('s')
}
function fprime(x) {
  return x
}
bug3(fprime);
bug3(function (x) {
  return x
});