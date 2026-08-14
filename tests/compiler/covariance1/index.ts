// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/covariance1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M {

    interface X { m1:number; }
    export class XX implements X { constructor(public m1:number) { } }

    interface Y { x:X; }

    export function f(y:Y) { }

    var a:X;
    f({x:a}); // ok
    //~^ ERROR: Variable 'a' is used before being assigned.

    var b:XX;
    f({x:b}); // ok covariant subtype
    //~^ ERROR: Variable 'b' is used before being assigned.
}
