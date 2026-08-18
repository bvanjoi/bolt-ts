// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalPropertiesSyntax.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface ifoo {
	x?:number;
	y:number;
}

class C1 implements ifoo {
	public y:number;
  //~^ ERROR: Property 'y' has no initializer and is not definitely assigned in the constructor.
}

class C2 implements ifoo { // ERROR - still need 'y'
  //~^ ERROR: Property 'y' is missing.
	public x:number;
  //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}

class C3 implements ifoo {
	public x:number;
  //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
	public y:number;
  //~^ ERROR: Property 'y' has no initializer and is not definitely assigned in the constructor.
}