// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericTypeAssertions1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A<T> { foo(x: T) { }}
var foo = new A<number>();
var r: A<string> = <A<number>>new A(); // error
//~^ ERROR: Type 'A<number>' is not assignable to type 'A<string>'.
var r2: A<number> = <A<A<number>>>foo; // error
//~^ ERROR: Type 'A<A<number>>' is not assignable to type 'A<number>'.
//~| ERROR: Conversion of type 'A<number>' to type 'A<A<number>>' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
