// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constructorInvocationWithTooFewTypeArgs.ts`, Apache-2.0 License

class D<T, U> {

  x: T
  //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.

  y: U
  //~^ ERROR: Property 'y' has no initializer and is not definitely assigned in the constructor.

}

var d = new D<number>();
//~^ ERROR: Expected 2 type arguments, but got 1.

var e = new D<number, number, number>()
//~^ ERROR: Expected 2 type arguments, but got 3.

