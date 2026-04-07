// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericConstraint3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
interface C<P> { x: P; }
interface A<T, U extends C<T>> { x: U; }
interface B extends A<{}, { x: {} }> { } // Should not produce an error