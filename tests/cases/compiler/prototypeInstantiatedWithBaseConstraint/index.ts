// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/prototypeInstantiatedWithBaseConstraint.ts`, Apache-2.0 License

//@ run-fail

class C<T> {
  x: T;
}

C.prototype.x.boo; // No error, prototype is instantiated to any