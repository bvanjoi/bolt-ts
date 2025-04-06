// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/infiniteExpandingTypeThroughInheritanceInstantiation.ts`, Apache-2.0 License

interface A<T>
{
   x: A<B<T>>
}

interface B<T> extends A<T> // error
{
   x: B<A<T>>
}
