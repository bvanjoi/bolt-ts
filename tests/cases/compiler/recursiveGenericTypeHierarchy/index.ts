// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveGenericTypeHierarchy.ts`, Apache-2.0 License

interface A<T extends A<T, S>, S extends A<T, S>> { }
interface B<T extends B<T, S>, S extends B<T, S>> extends A<B<T, S>, B<T, S>> { }