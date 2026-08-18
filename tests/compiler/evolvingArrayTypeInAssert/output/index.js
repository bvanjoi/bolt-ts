// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/evolvingArrayTypeInAssert.ts`, Apache-2.0 License
//@compiler-options: target=es2015
export function unsafeCast(_value) {}
function yadda() {
  var out = [];
  out.push(100);
  unsafeCast(out);
  return out;
}