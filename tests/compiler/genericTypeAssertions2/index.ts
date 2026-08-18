// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericTypeAssertions2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A<T> { foo(x: T) { } }
class B<T> extends A<T> {
    bar(): T {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'T'.
    }
}

var foo = new A<number>();
var r: A<string> = <B<string>>new B();
var r2: A<number> = <B<string>>new B(); // error
//~^ ERROR: Type 'B<string>' is not assignable to type 'A<number>'.
var r3: B<number> = <A<number>>new B(); // error
//~^ ERROR: Property 'bar' is missing.
var r4: A<number> = <A<number>>new A();
var r5: A<number> = <A<number>>[]; // error
//~^ ERROR: Property 'foo' is missing.
