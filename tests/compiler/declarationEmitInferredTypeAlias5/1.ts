import * as Z from "./0"
//let v2: Z.Data;
let v = "str" || true;
//~^ ERROR: This kind of expression is always truthy.
export { v }