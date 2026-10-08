// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/newExpressionWithTypeParameterConstrainedToOuterTypeParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I<T> {
    new <U extends T>(u: U): U;
}
var i: I<string>;
var y = new i(""); // y should be string
//~^ ERROR: Variable 'i' is used before being assigned.