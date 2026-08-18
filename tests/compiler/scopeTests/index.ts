// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/scopeTests.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C { private v; public p; static s; }
class D extends C {
  //~^ ERROR: Class 'D' incorrectly extends base class 'C'.
  public v: number;
  public p: number
  constructor() {
   super()
   this.v = 1;
   this.p = 1;
   C.s = 1;
  }
}