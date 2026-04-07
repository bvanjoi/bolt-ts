// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/topLevelLambda.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
	var f = () => {this.window;}
  //~^ ERROR: 'this' cannot be referenced in a module or namespace body.
}
