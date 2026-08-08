// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/scopeCheckInsideStaticMethod1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
   private v;
   public p;
   static s;
   static b() {
      v = 1; // ERR
      //~^ ERROR: Cannot find name 'v'.
      C.s = 1;
      this.p = 1; // ERR
      //~^ ERROR: Property 'p' does not exist on type 'typeof C'.
   }
}