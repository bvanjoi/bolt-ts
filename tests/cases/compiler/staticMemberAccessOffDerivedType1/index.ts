// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/staticMemberAccessOffDerivedType1.ts`, Apache-2.0 License

class SomeBase {
  static GetNumber() {
      return 2;
  }
}
class P extends SomeBase {
  static SomeNumber = P.GetNumber();
}
