// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonArrayRestArgs.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function foo(...rest: number) { // error
  //~^ ERROR: A rest parameter must be of an array type.
	var x: string = rest[0];
	return x;
}
