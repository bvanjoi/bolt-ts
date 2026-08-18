// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forLoopEndingMultilineComments.ts`, Apache-2.0 License

export function consoleTestResultHandler(testResult) {
  void a;
  for ( var q of a) {
    void a;
    if (a) {} else {}
    
  }
  return true;
}