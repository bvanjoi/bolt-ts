// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/missingTypeArguments1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I<T> { }
class Y<T> {}
class X<T> {
    p1: () => X;
    //~^ ERROR: Property 'p1' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: Generic type 'X<T>' requires 1 type argument.
}
var a: X<number>;

class X2<T> {
    p2: { [idx: number]: X2 } 
    //~^ ERROR: Property 'p2' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: Generic type 'X2<T>' requires 1 type argument.
}
var a2: X2<number>;

class X3<T> {
    p3: X3[]
    //~^ ERROR: Property 'p3' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: Generic type 'X3<T>' requires 1 type argument.
}
var a3: X3<number>;

class X4<T> {
    p4: I<X4>
    //~^ ERROR: Property 'p4' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: Generic type 'X4<T>' requires 1 type argument.
}
var a4: X4<number>;

class X5<T> {
    p5: X5
    //~^ ERROR: Generic type 'X5<T>' requires 1 type argument.
}
var a5: X5<number>;

class X6<T> {
    p6: () => Y;
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
    //~| ERROR: Property 'p6' has no initializer and is not definitely assigned in the constructor.
}
var a6: X6<number>;

class X7<T> {
    p7: { [idx: number]: Y } 
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
    //~| ERROR: Property 'p7' has no initializer and is not definitely assigned in the constructor.
}
var a7: X7<number>;

class X8<T> {
    p8: Y[]
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
    //~| ERROR: Property 'p8' has no initializer and is not definitely assigned in the constructor.
}
var a8: X8<number>;

class X9<T> {
    p9: I<Y>
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
    //~| ERROR: Property 'p9' has no initializer and is not definitely assigned in the constructor.
}
var a9: X9<number>;

class X10<T> {
    pa: Y
    //~^ ERROR: Generic type 'Y<T>' requires 1 type argument.
}
var a10: X10<number>;

 
