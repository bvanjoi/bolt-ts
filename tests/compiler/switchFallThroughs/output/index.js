// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/switchFallThroughs.ts`, Apache-2.0 License
function R1(index) {
  switch (index) {
    case 0:
    case 1:
    case 2:
      var a = 'a';
      return a;
    
    case 3:
    case 4:
      {
        return 'b';
      }
    
    case 5:
    default:
      return 'c';
    
  }
}