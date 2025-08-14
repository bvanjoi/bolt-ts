// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/staticAndMemberFunctions.ts`, Apache-2.0 License

class T {
  static x() { }
  public y() { }
}

T.x;
T.x();

(new T).y;
(new T()).y;

(new T).y();
(new T()).y();