let v = "str" || true;
//~^ ERROR: This kind of expression is always truthy.
function bar () {
    return v;
}
export { v, bar }