// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nonArrayRestArgs.ts`, Apache-2.0 License

function foo(...rest: number) { // error
  //~^ ERROR: A rest parameter must be of an array type.
	var x: string = rest[0];
	return x;
}