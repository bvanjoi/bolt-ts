// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nonConflictingRecursiveBaseTypeMembers.ts`, Apache-2.0 License

interface A<T> {
  x: C<T>
}

interface B<T> {
  x: C<T>
}

interface C<T> extends A<T>, B<T> { } // Should not be an error