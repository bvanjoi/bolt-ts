// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fallFromLastCase2.ts`, Apache-2.0 License
//@compiler-options: target=es6
//@compiler-options: strict=false
function foo1(a) {
  switch (a) {
    case 1:
      use('1');
      break;
    
    case 2:
      use('2');
    
    case 3:
      use('3');
    
  }
}
function foo2(a) {
  switch (a) {
    case 1:
      use('1');
      break;
    
    default:
      use('2');
    
    case 2:
      use('3');
    
  }
}