// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indirectTypeParameterReferences.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
var flowtypes = (b) => {
  var combined = (fn) => (null);
  var literal = (fn) => (null);
  return {
      combined,
    literal    
  };
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