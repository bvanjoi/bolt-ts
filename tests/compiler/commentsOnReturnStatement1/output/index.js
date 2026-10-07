// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsOnReturnStatement1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: removeComments=false
class DebugClass {
  static debugFunc() {
    var // Start Debugger Test Code
    i = 0;
    return // End Debugger Test Code
    true;
  }
}