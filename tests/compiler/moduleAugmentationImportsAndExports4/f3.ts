import {A} from "./f1";
import {B} from "./f2";

A.prototype.foo = function () { return undefined; }
//~^ ERROR: Type '() => undefined' is not assignable to type '() => B'.

namespace N {
    export interface Ifc { a: number; }
    export interface Cls { b: number; }
}
import I = N.Ifc;
import C = N.Cls;

declare module "./f1" {
    interface A {
        foo(): B;
        bar(): I;
        baz(): C;
    }
}