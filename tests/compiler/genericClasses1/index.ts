// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericClasses1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

class C<T> {
	public x: T;
  //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}

var v1 = new C<string>();

var y = v1.x; // should be 'string'