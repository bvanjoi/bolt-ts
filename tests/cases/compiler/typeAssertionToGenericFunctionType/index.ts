// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeAssertionToGenericFunctionType.ts`, Apache-2.0 License

var x = {
  a: < <T>(x: T) => T > ((x: any) => 1),
  b: <T>(x: T) => { x }
}
x.a<string>(1); // bug was that this caused 'Could not find symbol T' on return type T in the type assertion on x.a's definition
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
x.b<string>(); // error
//~^ ERROR: Expected 1 arguments, but got 0.