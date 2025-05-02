// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/setterWithReturn.ts`, Apache-2.0 License

class C234 {
  public set p1(arg1) {
      if (true) {
          return arg1; //~ERROR: Setters cannot return a value.
      }
      else {
          return 0;   //~ERROR: Setters cannot return a value.
      }
 }
}