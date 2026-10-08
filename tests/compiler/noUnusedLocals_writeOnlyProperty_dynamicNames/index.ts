// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noUnusedLocals_writeOnlyProperty_dynamicNames.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedLocals
//@compiler-options: lib=[es6]

const x = Symbol("x");
const y = Symbol("y");
class C {
    private [x]: number;
    //~^ ERROR: '[x]' is declared but its value is never read.
    private [y]: number;
    m() {
        this[x] = 0; // write-only
        this[y];
    }
}
