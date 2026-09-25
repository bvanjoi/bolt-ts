// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noUnusedLocals_writeOnlyProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedLocals

class C {
    private x;
    //~^ ERROR: 'x' is declared but its value is never read.
    m() {
        this.x = 0;
    }
}