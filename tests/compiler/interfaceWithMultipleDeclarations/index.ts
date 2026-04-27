// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/interfaceWithMultipleDeclarations.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I1<V> {
  //~^ ERROR: All declarations of 'I1' must have identical type parameters.
}
interface I1<S> { // Name mismatch
  //~^ ERROR: All declarations of 'I1' must have identical type parameters.
}
interface I1<T, U extends T> { // Length mismatch
  //~^ ERROR: All declarations of 'I1' must have identical type parameters.
}
interface I1<V extends string> { // constraint present
  //~^ ERROR: All declarations of 'I1' must have identical type parameters.
}
interface I1<V, X extends V> { // Length mismatch
  //~^ ERROR: All declarations of 'I1' must have identical type parameters.
}
interface I1 { // Length mismatch
  //~^ ERROR: All declarations of 'I1' must have identical type parameters.
}

interface I2<T extends string> {
  //~^ ERROR: All declarations of 'I2' must have identical type parameters.
}
interface I2<T extends () => string> { // constraint mismatch
  //~^ ERROR: All declarations of 'I2' must have identical type parameters.
}
interface I2<T> { // constraint absent
  //~^ ERROR: All declarations of 'I2' must have identical type parameters.
}
interface I2<U> { // name mismatch
  //~^ ERROR: All declarations of 'I2' must have identical type parameters.
}
interface I2<X, Y> { // length mismatch
  //~^ ERROR: All declarations of 'I2' must have identical type parameters.
}
interface I2 { // length mismatch
  //~^ ERROR: All declarations of 'I2' must have identical type parameters.
}

interface I3 {
  //~^ ERROR: All declarations of 'I3' must have identical type parameters.
}
interface I3<T> { // length mismatch
  //~^ ERROR: All declarations of 'I3' must have identical type parameters.
}

class Foo<T> {
}
interface I4<T extends Foo<T>> {
}
interface I4<T extends Foo<T>> { // Should not be error
}