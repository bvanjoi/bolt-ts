// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/anonymousModules.ts`, Apache-2.0 License

//@compiler-options: target=es2015

module {
  //~^ ERROR: Cannot find name 'module'.
  //~| ERROR: Unexpected keyword or identifier.
	export var foo = 1;
	//~^ ERROR: An export declaration can only be used at the top level of a namespace or module.

	module {
  //~^ ERROR: Cannot find name 'module'.
  //~| ERROR: Unexpected keyword or identifier.
		export var bar = 1;
		//~^ ERROR: An export declaration can only be used at the top level of a namespace or module.
	}

	var bar = 2;

	module {
  //~^ ERROR: Cannot find name 'module'.
  //~| ERROR: Unexpected keyword or identifier.
		var x = bar;
	}
}