// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/constEnumSyntheticNodesComments.ts`, Apache-2.0 License
var En = {};
(function (En) {

  En[En['A'] = 0] = 'A'
  En[En['B'] = 0] = 'B'
  En[En['C'] = 0] = 'C'
  En[En['D'] = 0] = 'D'
})(En);
function assert(x) {
  return x;
}
function verify(a) {
  switch (a) {
    case En.A:
      return assert(a);
    
    case En['B']:
      return assert(a);
    
    case En[`C`]:
      return assert(a);
    
    case En['D']:
      return assert(a);
    
  }
}