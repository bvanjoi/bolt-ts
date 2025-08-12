// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/noErrorsInCallback.ts`, Apache-2.0 License

class Bar {
  constructor(public foo: string) { }
}
var one = new Bar({}); // Error
//~^ ERROR: Argument of type '{ }' is not assignable to parameter of type 'string'.
[].forEach(() => {
  var two = new Bar({}); // No error?
  //~^ ERROR: Argument of type '{ }' is not assignable to parameter of type 'string'.
});
