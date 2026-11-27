import {A} from "./f1";
import "./f3";

let a: A;
let b = a.foo().n;
//~^ ERROR: Variable 'a' is used before being assigned.
let c = a.bar().a;
//~^ ERROR: Variable 'a' is used before being assigned.
let d = a.baz().b;
//~^ ERROR: Variable 'a' is used before being assigned.
