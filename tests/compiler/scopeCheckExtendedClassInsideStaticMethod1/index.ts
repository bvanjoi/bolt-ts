// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/scopeCheckExtendedClassInsideStaticMethod1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C { private v; public p; static s; }
class D extends C {
   static c() {
      v = 1;
      //~^ ERROR: Cannot find name 'v'.
      this.p = 1;
      //~^ ERROR: Property 'p' does not exist on type 'typeof D'.
      s = 1;
      //~^ ERROR: Cannot find name 's'.
   }
}
