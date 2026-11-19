// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/identityRelationNeverTypes.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration
// Repro from #47996
function f1(state) {
  if (state.matches('a') && state.matches('a.b')) {
    state;// never
    // true
    // true
    
  }
  
}