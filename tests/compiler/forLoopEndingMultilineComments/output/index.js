
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forLoopEndingMultilineComments.ts`, Apache-2.0 License
//@compiler-options: target=es2015
export function consoleTestResultHandler(testResult) {
  // needed to get colors to show up when passing through Grunt
  void a;
  for ( var q of a) {
    void a;
    /* eslint-disable no-console */
    if (a) {} else {}
    
  /* eslint-enable no-console */
  }
  return true;
}