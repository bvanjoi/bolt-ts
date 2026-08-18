// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericClasses2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

interface Foo<T> {
	a: T;
}

class C<T> {
	public x: T;
  //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
	public y: Foo<T>;
  //~^ ERROR: Property 'y' has no initializer and is not definitely assigned in the constructor.
	public z: Foo<number>;
  //~^ ERROR: Property 'z' has no initializer and is not definitely assigned in the constructor.
}

var v1 : C<string>;

var y = v1.x; // should be 'string'
//~^ ERROR: Variable 'v1' is used before being assigned.
var w = v1.y.a; // should be 'string'
//~^ ERROR: Variable 'v1' is used before being assigned.
var z = v1.z.a; // should be 'number'
//~^ ERROR: Variable 'v1' is used before being assigned.
