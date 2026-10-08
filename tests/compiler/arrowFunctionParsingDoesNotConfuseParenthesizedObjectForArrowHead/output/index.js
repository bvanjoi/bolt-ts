

// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arrowFunctionParsingDoesNotConfuseParenthesizedObjectForArrowHead.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var test = () => (({
  prop: !value,
  run: () => {
    if // "Identifier expected." error on "!" and two "Duplicate identifier '(Missing)'." errors on space.
    (!a.b// remove ! to see that errors will be gone
    ()) {
      return 'special'//replace arrow function with regular function to see that errors will be gone
      // comment next line or remove "()" to see that errors will be gone
      ;
    }
    
    return 'default';
  }  
}));