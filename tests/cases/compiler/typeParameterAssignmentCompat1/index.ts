// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeParameterAssignmentCompat1.ts`, Apache-2.0 License

interface Foo<T> {
  frobble(value: T): T;
}

function f<T, U>(): Foo<U> {
  var x: Foo<T>;
  var y: Foo<U>;
  x = y; // should be an error
  //~^ ERROR: Type 'Foo<U>' is not assignable to type 'Foo<T>'.
  return x; //~ ERROR: Type 'Foo<T>' is not assignable to type 'Foo<U>'.
}

class C<T> {
  f<U>(): Foo<U> {
      var x: Foo<T>;
      var y: Foo<U>;
      x = y; // should be an error
      //~^ ERROR: Type 'Foo<U>' is not assignable to type 'Foo<T>'.
      return x; //~ ERROR: Type 'Foo<T>' is not assignable to type 'Foo<U>'.
  }
}