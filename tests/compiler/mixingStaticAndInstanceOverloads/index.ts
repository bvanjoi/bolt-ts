// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mixingStaticAndInstanceOverloads.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C1 {
    // ERROR
    foo1(n: number);
    foo1(s: string);
    static foo1(a) { }
    //~^ ERROR: Function overload must not be static.
}
class C2 {
    // ERROR
    static foo2(n: number);
    static foo2(s: string);
    foo2(a) { }
    //~^ ERROR: Function overload must be static.
}
class C3 {
    // ERROR
    foo3(n: number);
    static foo3(s: string);
    //~^ ERROR: Function overload must not be static.
    foo3(a) { }
    //~^ ERROR: Function overload must be static.
}
class C4 {
    // ERROR
    static foo4(n: number);
    foo4(s: string);
    //~^ ERROR: Function overload must be static.
    static foo4(a) { }
    //~^ ERROR: Function overload must not be static.
}
class C5 {
    // OK
    foo5(n: number);
    foo5(s: string);
    foo5(a) { }

    // OK
    static foo5(n: number);
    static foo5(s: string);
    static foo5(a) { }
}