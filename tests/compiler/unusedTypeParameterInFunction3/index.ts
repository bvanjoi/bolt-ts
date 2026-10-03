// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParameterInFunction3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

function f1<X, Y, Z>() {
  //~^ ERROR: 'Y' is declared but its value is never read.
    var a!: X;
    var b!: Z;
    a;
    b;
}
