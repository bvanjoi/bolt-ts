// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedParametersInLambda1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class A {
    public f1() {
        return (X) => {
          //~^ ERROR: 'X' is declared but its value is never read.
        }
    }
}