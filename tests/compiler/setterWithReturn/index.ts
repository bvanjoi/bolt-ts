// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/setterWithReturn.ts`, Apache-2.0 License

class C234 {
  public set p1(arg1) {
  //~^ ERROR: Parameter 'arg1' implicitly has an 'any' type.
  //~| ERROR: Property 'p1' implicitly has type 'any', because its set accessor lacks a parameter type annotation.
      if (true) {
          return arg1; //~ERROR: Setters cannot return a value.
      }
      else {
          return 0;   //~ERROR: Setters cannot return a value.
      }
 }
}
