// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mismatchedGenericArguments1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface IFoo<T> {
   foo<T>(x: T): T;
}
class C<T> implements IFoo<T> {
   foo(x: string): number {
    //~^ ERROR: Property 'foo' in type 'C<T, C>' is not assignable to the same property in base type 'IFoo<T, C>'.
     return null;
     //~^ ERROR: Type 'null' is not assignable to type 'number'.
   }
}

class C2<T> implements IFoo<T> {
   foo<U>(x: string): number {
    //~^ ERROR: Property 'foo' in type 'C2<T, C2>' is not assignable to the same property in base type 'IFoo<T, C2>'.
     return null;
     //~^ ERROR: Type 'null' is not assignable to type 'number'.
   }
}