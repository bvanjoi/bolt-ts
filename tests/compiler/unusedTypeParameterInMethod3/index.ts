// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParameterInMethod3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class A {
    public f1<X, Y, Z>() {
      //~^ ERROR: 'Z' is declared but its value is never read.
        var a!: X;
        var b!: Y;
        a;
        b;
    }
}