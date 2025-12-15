// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/thisShadowingErrorSpans.ts`, Apache-2.0 License

//@compiler-options: strict

class C {
    m() {
        this.m();
        function f() {
            this.m();
            //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
        }
    }
}
