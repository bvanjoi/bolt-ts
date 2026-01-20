// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/specializedLambdaTypeArguments.ts`, Apache-2.0 License

class X<A> {
	prop: X< <Tany>() => Tany >;
}
var a: X<boolean>;