// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/scopeCheckExtendedClassInsidePublicMethod2.ts`, Apache-2.0 License

class C { private v; public p; static s; }
class D extends C {
   public c() {
      v = 1;
      //~^ ERROR: Cannot find name 'v'.
      this.p = 1;
      s = 1;
      //~^ ERROR: Cannot find name 's'.
   }
}
