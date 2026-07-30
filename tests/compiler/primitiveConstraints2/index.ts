// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/primitiveConstraints2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C<T> {
   public bar2<U extends T>(x: T, y: U): T {
      return null;
      //~^ ERROR: Type 'null' is not assignable to type 'T'.
     }
}
 
var x = new C<number>();
x.bar2(2, ""); // should error
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
x.bar2<string>(2, ""); // should error
//~^ ERROR: Type 'string' does not satisfy the constraint 'number'.
