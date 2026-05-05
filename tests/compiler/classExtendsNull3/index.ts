// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classExtendsNull3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

class C1 extends null {
  static method() {
    super.oops;
    //~^ ERROR: 'super' is possibly 'null'.
  }
}

class C2 extends null {
  method() {
    super.oops;
    //~^ ERROR: 'super' is possibly 'null'.
  }
}