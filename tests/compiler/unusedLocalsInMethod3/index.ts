// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsInMethod3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters


class greeter {
    public function1() {
        var x, y = 10;
        //~^ ERROR: All Variables are unused.
        y = 1;
    }
}
