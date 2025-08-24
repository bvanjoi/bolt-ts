// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/instanceSubtypeCheck1.ts`, Apache-2.0 License

interface A<T>
{
   x: A<B<T>>
}
 
interface B<T> extends A<T>
{
   x: B<A<T>>
}