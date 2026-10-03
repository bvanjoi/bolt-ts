// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParameterInFunction2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

function f1<X, Y>() {
  //~^ ERROR: 'Y' is declared but its value is never read.
    var a!: X;
    a;
}
