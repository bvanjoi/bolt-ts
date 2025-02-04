class C<T extends C<T>> {
  foo<U extends C<C<T>>(x: U) { //~ ERROR: Expected '>'.
      return null;
  }
}