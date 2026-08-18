// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/scopeCheckExtendedClassInsidePublicMethod2.ts`, Apache-2.0 License

class C { private v; public p; static s; }
//~^ ERROR: Member 'v' implicitly has an 'any' type.
//~| ERROR: Member 'p' implicitly has an 'any' type.
//~| ERROR: Member 's' implicitly has an 'any' type.
class D extends C {
   public c() {
      v = 1;
      //~^ ERROR: Cannot find name 'v'.
      this.p = 1;
      s = 1;
      //~^ ERROR: Cannot find name 's'.
   }
}
