// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reachabilityCheckWithEmptyDefault.ts`, Apache-2.0 License
function foo(x) {
  switch (x) {
    case 1:
      return ;
    
    default:
  }
  print('1');
}