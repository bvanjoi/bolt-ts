// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/scopeCheckInsidePublicMethod1.ts`, Apache-2.0 License

class C {
  static s;
  public a() {
     s = 1; //~ ERROR: Cannot find name 's'.
  }
}