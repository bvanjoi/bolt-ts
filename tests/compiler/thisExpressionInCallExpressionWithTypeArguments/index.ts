// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisExpressionInCallExpressionWithTypeArguments.ts`, Apache-2.0 License

class C {
  public foo() { [1,2,3].map<any,any>((x) => { return this; })}
  //~^ ERROR: Expected 1 type arguments, but got 2.
}
