// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/raiseErrorOnParameterProperty.ts`, Apache-2.0 License

class C1 {
  constructor(public x: X) {
    //~^ ERROR: Cannot find name 'X'.
  }
}
var c1 = new C1(0);
 
