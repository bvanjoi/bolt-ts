// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleExportsUnaryExpression.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var x = 1;
export function foo(y) {
  if (y <= x++) return y <= x++;
  
  if (y <= x--) return y <= x--;
  
  if (y <= ++x) return y <= ++x;
  
  if (y <= --x) return y <= --x;
  
  x++;
  x--;
  ++x;
  --x;
}
export { x }