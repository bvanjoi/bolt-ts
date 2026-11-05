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