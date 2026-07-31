// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadBindingAcrossDeclarationBoundaries.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Opt1 {
    p?: any;
}
interface Opt2 {
    q?: any;
}
interface Opt3 {
    r?: any;
}
interface Opt4 {
    s?: any;
}
interface A {
    a(o: Opt1): Opt1;
    a(o: Opt2): Opt2;
    (o: Opt1): Opt1;
    (o: Opt2): Opt2;
    new (o: Opt1): Opt1;
    new (o: Opt2): Opt2;
}
interface A {
    a(o: Opt3): Opt3;
    a(o: Opt4): Opt4;
    (o: Opt3): Opt3;
    (o: Opt4): Opt4;
    new (o: Opt3): Opt3;
    new (o: Opt4): Opt4;
}

var a: A;
// These should all be Opt3
var a1 = a.a({});
//~^ ERROR: Variable 'a' is used before being assigned.
//~| ERROR: Variable 'a' is used before being assigned.
var a1 = a({});
//~^ ERROR: Variable 'a' is used before being assigned.
//~| ERROR: Variable 'a' is used before being assigned.
var a1 = new a({});
//~^ ERROR: Variable 'a' is used before being assigned.
