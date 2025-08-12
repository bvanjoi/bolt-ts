// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/infinitelyExpandingBaseTypes2.ts`, Apache-2.0 License

interface A<T>
{
   x : A<()=>T>
}
 
interface B<T>
{
   x : B<()=>T>
}
 
var a: A<string>
var b: B<string> = a
