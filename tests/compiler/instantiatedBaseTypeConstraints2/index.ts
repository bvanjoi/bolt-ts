// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/instantiatedBaseTypeConstraints2.ts`, Apache-2.0 License

interface A<T extends A<T, S>, S extends A<T, S>> { }
interface B<U> extends A<B<U>, B<U>> { }