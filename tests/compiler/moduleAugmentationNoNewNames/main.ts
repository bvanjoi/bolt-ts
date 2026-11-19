import { Observable } from "./observable"
import "./map";

let x: Observable<number>;
let y = x.map(x => x + 1);
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'x' is used before being assigned.