// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedPrivateMethodInClass1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class greeter {
    private function1() {
      //~^ ERROR: 'function1' is declared but its value is never read.
        var y = 10;
      //~^ ERROR: 'y' is declared but its value is never read.
    }
}