// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/constructorInvocationWithTooFewTypeArgs.ts`, Apache-2.0 License

class D<T, U> {

  x: T

  y: U

}

var d = new D<number>();
//~^ ERROR: Expected 2 type arguments, but got 1.

var e = new D<number, number, number>()
//~^ ERROR: Expected 2 type arguments, but got 3.

