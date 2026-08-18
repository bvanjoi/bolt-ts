// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypedSymbolNamedProperties.ts`, Apache-2.0 License
//@compiler-options: strict
//@compiler-options: target=esnext
//@compiler-options: declaration
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