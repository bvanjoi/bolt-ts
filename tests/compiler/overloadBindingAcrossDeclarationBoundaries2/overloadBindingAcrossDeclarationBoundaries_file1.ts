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