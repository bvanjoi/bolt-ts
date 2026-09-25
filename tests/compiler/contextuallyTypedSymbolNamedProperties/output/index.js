var A = Symbol('A');
var B = Symbol('B');

f(ab, {
  [A]: (ap) => {
    ap.description;
  },
  [B]: (bp) => {
    bp.description;
  }  
});
var x = {
  [A]: (s) => (s.length)  
};