import {A} from "./f1";
import {B} from "./f2";

A.prototype.foo = function () { return undefined; }
//~^ ERROR: Type '() => undefined' is not assignable to type '() => B'.
declare module "./f1" {
    interface A {
        foo(): B;
    }
}