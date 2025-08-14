// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/infinitelyExpandingBaseTypes1.ts`, Apache-2.0 License

interface A<T>
{
    x : A<A<T>>
}
 
interface B<T>
{
    x : B<T>
}
 
interface C<T> extends A<T>, B<T> { }

 
