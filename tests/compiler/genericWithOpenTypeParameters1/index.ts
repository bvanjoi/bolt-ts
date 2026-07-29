// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericWithOpenTypeParameters1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class B<T> {
   foo(x: T): T { return null; }
   //~^ ERROR: Type 'null' is not assignable to type 'T'.
}

declare var x: B<number>;
x.foo(1); // no error
var f = <T>(x: B<T>) => { return x.foo(1); } // error
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'T'.
var f2 = <T>(x: B<T>) => { return x.foo<T>(1); } // error
//~^ ERROR: Expected 0 type arguments, but got 1.
var f3 = <T>(x: B<T>) => { return x.foo<number>(1); } // error
//~^ ERROR: Expected 0 type arguments, but got 1.
var f4 = (x: B<number>) => { return x.foo(1); } // no error
