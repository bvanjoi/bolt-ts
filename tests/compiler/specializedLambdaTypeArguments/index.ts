// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/specializedLambdaTypeArguments.ts`, Apache-2.0 License

class X<A> {
	prop: X< <Tany>() => Tany >;
	//~^ ERROR: Property 'prop' has no initializer and is not definitely assigned in the constructor.
}
var a: X<boolean>;
