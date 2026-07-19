// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/prototypeInstantiatedWithBaseConstraint.ts`, Apache-2.0 License

//@ run-fail

class C<T> {
  x: T;
  //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}

C.prototype.x.boo; // No error, prototype is instantiated to any
