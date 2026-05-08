var flowtypes = (b) => {
  var combined = (fn) => (null);
  var literal = (fn) => (null);
  return {
      combined,
    literal    
  }
};
var {combined, literal} = flowtypes({
  b: 'b-value'  
});
literal((aPlusB) => {
  aPlusB.b;
  aPlusB.a;
});
combined((comb) => {
  comb.b;
  comb.a;
});
var n = f(2).a;