// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restParameterTypeInstantiation.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
// Repro from #33823
var removeF = ({f, ...rest}) => (rest);
var result = removeF({
  f: '',
  g: 3  
}).g;