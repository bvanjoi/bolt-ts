// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implementPublicPropertyAsPrivate.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I {
    x: number;
}
class C implements I {
  //~^ ERROR:  Class 'C' incorrectly implements interface 'I'.
    private x = 0; // should raise error at class decl
}
