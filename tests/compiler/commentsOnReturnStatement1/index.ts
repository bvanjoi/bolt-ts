// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsOnReturnStatement1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: removeComments=false

class DebugClass {
    public static debugFunc() {
        // Start Debugger Test Code
        var i = 0;

        // End Debugger Test Code
        return true;
    }
}