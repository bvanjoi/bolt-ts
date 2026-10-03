export function unsafeCast(_value) {}
function yadda() {
  var out = [];
  out.push(100);
  unsafeCast(out);
  return out;
}