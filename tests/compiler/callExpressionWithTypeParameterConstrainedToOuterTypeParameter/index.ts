// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/callExpressionWithTypeParameterConstrainedToOuterTypeParameter.ts`, Apache-2.0 License

interface I<T> {
  <U extends T>(u: U): U;
}
var i: I<string>;
var y = i(""); // y should be string
//~^ ERROR: Variable 'i' is used before being assigned.
//~| ERROR: Variable 'i' is used before being assigned.
var y0: string = i("");
//~^ ERROR: Variable 'i' is used before being assigned.
var z = i(42);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
//~| ERROR: Variable 'i' is used before being assigned.
//~| ERROR: Variable 'i' is used before being assigned.
