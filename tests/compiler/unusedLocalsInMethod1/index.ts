// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsInMethod1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class greeter {
    public function1() {
        var x = 10;
        //~^ ERROR: 'x' is declared but its value is never read.
    }
}
