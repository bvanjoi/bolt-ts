// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unknownTypeArgOnCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Foo<T> {
  public clone<U>() {
   return null;
   }
}
var f = new Foo<number>();
var r = f.clone<Uhhhh>()
//~^ ERROR: Cannot find name 'Uhhhh'.
