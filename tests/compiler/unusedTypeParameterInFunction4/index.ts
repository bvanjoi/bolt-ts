// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParameterInFunction4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

function f1<X, Y, Z>() {
  //~^ ERROR: 'X' is declared but its value is never read.
    var a!: Y;
    var b!: Z;
    a;
    b;
}