// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsAfterCaseClauses1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function getSecurity(level) {
  switch (level) {
    case 0:
    case 1:
    case 2:
      return 'Hi';
    
    case 3:
    case 4:
      return 'hello';
    
    case 5:
    default:
      return 'world';
    
  }
}