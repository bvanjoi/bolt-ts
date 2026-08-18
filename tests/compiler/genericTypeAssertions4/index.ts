// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericTypeAssertions4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
    foo() { return ""; }
}

class B extends A {
    bar() { return 1; }
}

class C extends A {
    baz() { return 1; }
}

var a: A;
var b: B;
var c: C;

function foo2<T extends A>(x: T) {
    var y = x;
    y = a; // error: cannot convert A to T
    //~^ ERROR: Type 'A' is not assignable to type 'T'.
    y = b; // error: cannot convert B to T
    //~^ ERROR: Type 'B' is not assignable to type 'T'.
    y = c; // error: cannot convert C to T
    //~^ ERROR: Type 'C' is not assignable to type 'T'.
    y = <T>a;
    y = <T>b; // error: cannot convert B to T
    //~^ ERROR: Conversion of type 'B' to type 'T' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
    y = <T>c; // error: cannot convert C to T
    //~^ ERROR: Conversion of type 'C' to type 'T' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
}
