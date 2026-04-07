// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/wrappedRecursiveGenericType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface X<T> { e: T; }
interface A<T> {
    a: B<T>;
    val: T;
}
interface B<T> {
    b: A<X<T>>;
    val: T;
}
declare var x: A<number>;
x.val = 5;         // val -> number
x.a.val = 5;       // val -> number
x.a.b.val = 5;     // val -> X<number> (This should be an error)
//~^ ERROR: Type 'number' is not assignable to type 'X<number>'.
x.a.b.a.val = 5;   // val -> X<number> (This should be an error)
//~^ ERROR: Type 'number' is not assignable to type 'X<number>'.