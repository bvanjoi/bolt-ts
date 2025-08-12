// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/staticInheritance.ts`, Apache-2.0 License

function doThing(x: { n: string }) { }
class A {
    static n: string;
    p = doThing(A); // OK
}
class B extends A {
    p1 = doThing(A); // OK
    p2 = doThing(B); // OK
}
doThing(B); //OK
