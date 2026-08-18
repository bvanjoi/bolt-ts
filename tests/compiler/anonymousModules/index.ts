// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/anonymousModules.ts`, Apache-2.0 License

//@compiler-options: target=es2015

module {
  //~^ ERROR: Cannot find name 'module'.
  //~| ERROR: Unexpected keyword or identifier.
	export var foo = 1;

	module {
  //~^ ERROR: Cannot find name 'module'.
  //~| ERROR: Unexpected keyword or identifier.
		export var bar = 1;
	}

	var bar = 2;

	module {
  //~^ ERROR: Cannot find name 'module'.
  //~| ERROR: Unexpected keyword or identifier.
		var x = bar;
	}
}