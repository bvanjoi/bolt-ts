// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericTypeAssertions6.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class A<T,U> {
    constructor(x) {
        var y = <T>x;
        var z = <U>x;
    }

    f(x: T, y: U) {
        x = <T>y;
      //~^ ERROR: Conversion of type 'U' to type 'T' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
        y = <U>x;
      //~^ ERROR: Conversion of type 'T' to type 'U' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
    }
}

class B<T extends Date, U extends Date> extends A<T, U> {
    g(x: T) {
        var a: Date = x;
        var b = <Date>x;
        var c = <T>new Date();
        var d = <U>new Date();
        var e = <T><U>new Date();
        //~^ ERROR: Conversion of type 'U' to type 'T' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
    }
}

declare var b: B<Date, Date>;
var c: A<Date, Date> = <A<Date, Date>>b;